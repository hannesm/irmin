open Lwt.Infix
module Store = Irmin_mem.KV (Irmin.Contents.String)

module Server =
  Irmin_graphql.Server.Make
    (Cohttp_lwt_unix.Server)
    (struct
      let remote = None

      let info ?(author = "graphql-test-author") =
        Format.kasprintf (fun msg () -> Irmin.Info.v ~date:0L ~author msg)
    end)
    (Store)

let port = 37634

let resolvers =
  Conduit_lwt.add Conduit_lwt.TCP.protocol
    (Conduit_lwt.TCP.resolve ~port)
    Conduit_lwt.empty

(** Create a GraphQL server over the supplied Irmin repository, returning the
    event loop thread. *)
let server_of_repo : type a. Store.repo -> a Lwt.t =
 fun repo ->
  let server = Server.v repo in
  let on_exn = raise in
  let cfg =
    {
      Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_any, port);
      Conduit_lwt.TCP.capacity = 40;
    }
  in
  Cohttp_lwt_unix.Server.create ~on_exn cfg Conduit_lwt.TCP.protocol
    Conduit_lwt.TCP.service server ()
  >>= fun () -> Lwt.fail_with "GraphQL server terminated unexpectedly"

type server = {
  event_loop : 'a. 'a Lwt.t;
  set_tree : Store.Tree.concrete -> unit Lwt.t;
}

let spawn_graphql_server () =
  let config = Irmin_mem.config () in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >|= fun master ->
  let event_loop = server_of_repo repo
  and set_tree tree =
    Store.Tree.of_concrete tree
    |> Store.set_tree_exn ~info:Irmin.Info.none master []
  in
  { event_loop; set_tree }

(** Issue a query to the localhost server and return the body of the response
    message *)
let send_query : string -> (string, [ `Msg of string ]) result Lwt.t =
 fun query ->
  let headers = Cohttp.Header.init_with "Content-Type" "application/json"
  and body =
    Yojson.Safe.to_string (`Assoc [ ("query", `String query) ])
    |> Cohttp_lwt.Body.of_string
  in
  Cohttp_lwt_unix.Client.post ~resolvers ~headers ~body
    (Uri.make ~scheme:"http" ~host:"localhost" ~port ~path:"graphql" ())
  >>= fun (response, body) ->
  let status = Cohttp_lwt.Response.status response in
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Code.(status |> code_of_status |> is_success) with
  | true -> Ok body
  | false ->
      let msg =
        Format.sprintf "Response: %s\nBody:\n%s"
          (Cohttp.Code.string_of_status status)
          body
      in
      Error (`Msg msg)
