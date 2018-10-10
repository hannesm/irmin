(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring
open Lwt.Infix

module Info (C: Mirage_clock.PCLOCK) = struct
  let f ~author c fmt =
    Fmt.kstrf (fun msg () ->
        C.now_d_ps c |>
        Ptime.v |> Ptime.to_float_s |> Int64.of_float |> fun date ->
        Irmin.Info.v ~date ~author msg
      ) fmt
end

module type S = sig
  include Irmin_git.S with type Private.Sync.endpoint = Git_mirage.endpoint
  val remote:
    ?conduit:Conduit_mirage.conduit ->
    ?resolver:Resolver_lwt.t ->
    ?headers:Cohttp.Header.t ->
    string -> Irmin.remote
end

module Git = struct

  module Make
      (G: Irmin_git.G)
      (C: Irmin.Contents.S)
      (P: Irmin.Path.S)
      (B: Irmin.Branch.S)
  = struct
    include Irmin_git.Make(G)(Git_mirage.Sync(G))(C)(P)(B)
    let remote ?conduit ?resolver ?headers uri =
      let e =
        Git_mirage.endpoint ?headers ?conduit ?resolver (Uri.of_string uri)
      in
      E e
  end

  module Ref (G: Irmin_git.G) (C: Irmin.Contents.S) = struct
    include Irmin_git.Ref(G)(Git_mirage.Sync(G))(C)
    let remote ?conduit ?resolver ?headers uri =
      let e =
        Git_mirage.endpoint ?headers ?conduit ?resolver (Uri.of_string uri)
      in
      E e
  end

  module KV (G: Irmin_git.G) (C: Irmin.Contents.S) = struct
    include Irmin_git.KV(G)(Git_mirage.Sync(G))(C)
    let remote ?conduit ?resolver ?headers uri =
      let e =
        Git_mirage.endpoint ?headers ?conduit ?resolver (Uri.of_string uri)
      in
      E e

  end

  module type KV_RO = sig
    type git
    include Mirage_kv_lwt.RO
    val connect:
      ?depth:int ->
      ?branch:string ->
      ?root:key ->
      ?conduit:Conduit_mirage.t ->
      ?resolver:Resolver_lwt.t ->
      ?headers:Cohttp.Header.t ->
      git -> string -> t Lwt.t
  end

  module KV_RO (G: Git.S) = struct

    module Key = Mirage_kv.Key

    type key = Key.t
    type value = string

    module G = struct
      include G
      let v ?dotgit:_ ?compression:_ ?buffers:_ _root =
        assert false
    end

    module S = KV(G)(Irmin.Contents.String)

    module Sync = Irmin.Sync(S)

    type 'a io = 'a Lwt.t
    let disconnect _ = Lwt.return_unit

    type error = [ Mirage_kv.error | `Exn of exn ]

    let pp_error ppf = function
      | #Mirage_kv.error as e -> Mirage_kv.pp_error ppf e
      | `Exn e -> Fmt.exn ppf e

    let err e: ('a, error) result = Error e
    let err_exn e = err (`Exn e)
    let err_not_found k = err (`Not_found k)

    let path x =
      (* XXX(samoht): we should probably just push the Key module in
         Irmin and remove the path abstraction completely ... *)
      Key.segments x

    module Tree = struct

      type t = { repo: S.repo; tree: S.tree }

      let digest t key =
        S.Tree.find_tree t.tree (path key) >>= function
        | None      -> Lwt.return (err_not_found key)
        | Some tree ->
          (* XXX(samoht): it shouldn't be blocking here and it the
             t.repo shouldn't be there. Need the RAO changes first. *)
          S.Tree.hash t.repo tree >|= fun h ->
          Ok (Irmin.Type.to_string S.Tree.hash_t h)

      let list t key =
        Lwt.catch (fun () ->
            S.Tree.list t.tree (path key) >|= fun l ->
            let l =
            List.map (fun (s, k) -> s, match k with
              | `Contents -> `Value
              | `Node     -> `Dictionary
              ) l
          in
          Ok l
        ) (fun e -> Lwt.return (err_exn e))

      let exists t key =
        Lwt.catch (fun () ->
            S.Tree.kind t.tree (path key) >|= function
            | Some `Contents -> Ok (Some `Value)
            | Some `Node     -> Ok (Some `Dictionary)
            | None           -> Ok None
          ) (fun e -> Lwt.return (err_exn e))

      let get t key =
        S.Tree.find t.tree (path key) >|= function
        | None   -> err_not_found key
        | Some v -> Ok v

    end

    type t = { root: S.key; t: S.t }

    let head_message t =
      S.Head.find t.t >|= function
      | None   -> "empty HEAD"
      | Some h ->
        let info = S.Commit.info h in
        Fmt.strf
          "commit: %a\n\
           Author: %s\n\
           Date: %Ld\n\
           \n\
           %s\n"
          S.Commit.pp_hash h
          (Irmin.Info.author info)
          (Irmin.Info.date info)
          (Irmin.Info.message info)


    (* XXX(samoht): extracted from Canopy. Should probably be
         upstreamed directly. *)

    module Topological = Graph.Topological.Make(S.History)

    (* XXX(samoht): very slow: will go through the entire history!! *)
    let created_updated_ids commit key =
      S.of_commit commit >>= fun t ->
      S.history t >>= fun history ->
      let aux commit_id acc =
        S.of_commit commit_id >>= fun store ->
        acc >>= fun (created, updated, last) ->
        S.find store (path key) >|= fun data ->
        match data, last with
        | None  , None -> (created, updated, last)
        | None  , Some _ -> (created, updated, last)
        | Some x, Some y when x = y -> (created, updated, last)
        | Some _, None -> (commit_id, commit_id, data)
        | Some _, Some _ -> (created, commit_id, data)
      in
      Topological.fold aux history (Lwt.return (commit, commit, None))

    let last_modified_at_commit head key =
      created_updated_ids head key  >|= fun (_, updated, _) ->
      S.Commit.info updated |> fun info ->
      (* XXX(samoht): should Irmin use Ptime directly? *)
      Ok (0, Irmin.Info.date info)

    let last_modified t key =
      S.Head.get t.t >>= fun head ->
      last_modified_at_commit head key

    let connect ?(depth = 1) ?(branch = "master")
        ?(root = Mirage_kv.Key.empty) ?conduit ?resolver ?headers
        t uri =
      let remote = S.remote ?conduit ?resolver ?headers uri in
      let head = G.Reference.of_string ("refs/heads/" ^ branch) in
      S.repo_of_git ~bare:true ~head t >>= fun repo ->
      S.of_branch repo branch >>= fun t ->
      Sync.pull_exn t ~depth remote `Set >|= fun () ->
      let root = path root in
      { t; root }

    let tree t =
      let repo = S.repo t.t in
      (S.find_tree t.t t.root >|= function
        | None      -> S.Tree.empty
        | Some tree -> tree)
      >|= fun tree ->
      { Tree.repo; tree }

    let exists t k = tree t >>= fun t -> Tree.exists t k
    let get t k = tree t >>= fun t -> Tree.get t k
    let list t k = tree t >>= fun t -> Tree.list t k
    let digest t k = tree t >>= fun t -> Tree.digest t k
  end

  module type KV_RW = sig

    type git
    type clock

    include Mirage_kv_lwt.RW

    val connect:
      ?depth:int ->
      ?branch:string ->
      ?root:key ->
      ?conduit:Conduit_mirage.t ->
      ?resolver:Resolver_lwt.t ->
      ?headers:Cohttp.Header.t ->
      ?author:string ->
      ?msg:([`Set of key| `Remove of key| `Batch] -> string) ->
      git -> clock -> string -> t Lwt.t

  end

  module KV_RW (G: Irmin_git.G) (C: Mirage_clock.PCLOCK) = struct

    (* XXX(samoht): batches are stored in memory. This could be bad if
       large objects are stored for too long... Might be worth having
       a clever LRU, which pushes larges objects to the underlying
       layer when needed.  *)

    type clock = C.t
    module Info = Info(C)
    module RO = KV_RO(G)
    module S = RO.S
    module Tree = RO.Tree

    type store =
      | Batch of { repo: S.repo; mutable tree: S.tree; origin: S.commit }
      | Store of RO.t

    and t = {
      store : store;
      author: string;
      clock : C.t;
      msg   : [`Set of RO.key|`Remove of RO.key|`Batch] -> string
    }

    type key = RO.key
    type value = RO.value
    type error = RO.error
    let pp_error = RO.pp_error
    type 'a io = 'a RO.io

    let default_author = "irmin <irmin@mirage.io>"

    let default_msg = function
      | `Set k    -> Fmt.strf "Updating %a" Mirage_kv.Key.pp k
      | `Remove k -> Fmt.strf "Removing %a" Mirage_kv.Key.pp k
      | `Batch    -> "Commmiting batch operation"

    let connect ?depth ?branch ?root ?conduit ?resolver ?headers
        ?(author = default_author)
        ?(msg = default_msg)
        git clock uri =
      RO.connect ?depth ?branch ?root ?conduit ?resolver ?headers git uri
      >|= fun t ->
      { store = Store t; author; clock; msg }

    let disconnect t = match t.store with
      | Store t -> RO.disconnect t
      | Batch _ -> Lwt.return ()

    (* XXX(samoht): always return the 'last modified' on the
       underlying storage layer, not for the current batch. *)
    let last_modified t k = match t.store with
      | Store t -> RO.last_modified t k
      | Batch b ->
        (* XXX(samoht): we need a Tree.last_modified function too *)
        RO.last_modified_at_commit b.origin k

    let repo t = match t.store with
      | Store t -> S.repo t.t
      | Batch b -> b.repo

    let tree t = match t.store with
      | Store t -> RO.tree t
      | Batch b -> Lwt.return { Tree.tree = b.tree; repo = repo t }

    let digest t k = tree t >>= fun t -> Tree.digest t k
    let exists t k = tree t >>= fun t -> Tree.exists t k
    let get t k = tree t >>= fun t -> Tree.get t k
    let list t k = tree t >>= fun t -> Tree.list t k

    type write_error = [ RO.error| Mirage_kv.write_error ]

    let pp_write_error ppf = function
      | #RO.error as e -> RO.pp_error ppf e
      | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e

    let info t op = Info.f ~author:t.author t.clock "%s" (t.msg op)

    let path = RO.path
    let err_exn e = (RO.err_exn e :> ('a, write_error) result)

    let set t k v =
      let info = info t (`Set k) in
      match t.store with
      | Store t ->
        Lwt.catch
          (fun () ->
             S.set ~info t.t (path k) v ~strategy:`Set >|= fun () -> Ok ())
          (fun e ->
             Lwt.return (err_exn e))
      | Batch b ->
        Lwt.catch
          (fun () ->
             S.Tree.add b.tree (path k) v >|= fun tree ->
             b.tree <- tree;
             Ok ())
          (fun e ->
             Lwt.return (err_exn e))


    let remove t k =
      let info = info t (`Remove k) in
      match t.store with
      | Store t ->
        Lwt.catch
          (fun () ->
             S.remove ~info t.t (path k) ~strategy:`Set >|= fun () -> Ok ())
          (fun e ->
             Lwt.return (err_exn e))
      | Batch b ->
        Lwt.catch
          (fun () ->
             S.Tree.remove b.tree (path k) >|= fun tree ->
             b.tree <- tree;
             Ok ())
          (fun e ->
             Lwt.return (err_exn e))

    let batch ?retries:_ t f =
      let info = info t `Batch in
      let rec aux t = match t.store with
        | Batch _ -> assert false
        | Store t ->
          let repo = S.repo t.t in
          S.Head.find t.t >>= function
          | None        -> assert false
          | Some origin ->
            S.Commit.tree origin >>= fun tree ->
            (S.Tree.find_tree tree t.root >|= function
             | Some t -> t
             | None   -> S.Tree.empty)
            >>= fun tree ->
            let batch = Batch { repo; tree; origin } in
            f batch >>= fun new_batch ->
            S.set_tree t.t ~info ~strategy:(`Merge_with_parent origin)
              t.root new_batch
      in
      aux t

  end

  module Mem = struct
    module G     = Irmin_git.Mem
    module Make  = Make(G)
    module Ref   = Ref(G)
    module KV    = KV(G)
    module KV_RO = KV_RO(G)
    module KV_RW = KV_RW(G)
  end

end
