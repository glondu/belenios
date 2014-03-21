(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2014 Inria                                           *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Signatures
open Util
open Serializable_t
open Web_serializable_j
open Lwt
open Web_common
open Web_signatures

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

module EMap = Map.Make(Uuidm)

module AclSet = Set.Make(struct
  type t = Web_serializable_t.acl
  let compare = compare
end)

let ( / ) = Filename.concat

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let populate accu f s = Lwt_stream.fold_s f s accu

let datadirs = ref []
let source_file = ref None
let main_election = ref None
let auth_instances = ref []

let () = CalendarLib.Time_Zone.(change Local)

let () =
  Eliom_config.get_config () |>
  let open Simplexmlparser in
  List.iter @@ function
  | PCData x ->
    Ocsigen_extensions.Configuration.ignore_blank_pcdata ~in_tag:"belenios" x
  | Element ("log", ["file", file], []) ->
    Lwt_main.run (open_security_log file)
  | Element ("source", ["file", file], []) ->
    source_file := Some file
  | Element ("data", ["dir", dir], []) ->
    datadirs := dir :: !datadirs
  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
    set_rewrite_prefix ~src ~dst
  | Element ("main-election", ["uuid", uuid], []) ->
    main_election := Some uuid
  | Element ("auth", ["name", auth_instance],
             [Element (auth_system, auth_config, [])]) ->
    let open Auth_common in
    let i = {auth_system; auth_instance; auth_config} in
    auth_instances := i :: !auth_instances
  | Element (tag, _, _) ->
    Printf.ksprintf failwith
      "invalid configuration for tag %s in belenios"
      tag

let main_election = match !main_election with
  | None -> None
  | Some u ->
    match Uuidm.of_string u with
    | Some u -> Some u
    | None -> failwith "Incorrect UUID in configuration <main-election> tag"

let get_single_line x =
  match_lwt Lwt_stream.get x with
  | None -> return None
  | Some _ as line ->
    lwt b = Lwt_stream.is_empty x in
    if b then (
      return line
    ) else (
      Lwt_stream.junk_while (fun _ -> true) x >>
      return None
    )

let process_datadir dir =
  Ocsigen_messages.debug (fun () ->
    "Using data from " ^ dir ^ "..."
  );
  lwt index =
    Lwt_io.chars_of_file (dir/"index.json") |>
    Lwt_stream.to_string >>=
    wrap1 datadir_index_of_string
  in
  Lwt_list.map_p (fun item ->
    let subdir = item.datadir_dir in
    let path = dir/subdir in
    let params_fname = path/"election.json" in
    let public_keys_fname = path/"public_keys.jsons" in
    Ocsigen_messages.debug (fun () ->
      "-- registering " ^ subdir
    );
    lwt raw_election =
      Lwt_io.lines_of_file params_fname |>
      get_single_line >>=
      (function
      | Some e -> return e
      | None -> failwith "election.json is invalid")
    in
    lwt metadata =
      let fname = path/"metadata.json" in
      lwt b = file_exists fname in
      if b then (
        Lwt_io.chars_of_file fname |>
        Lwt_stream.to_string >>=
        wrap1 metadata_of_string
      ) else return empty_metadata
    in
    let public_creds_fname = path/"public_creds.txt" in
    lwt public_creds =
      Lwt_io.lines_of_file public_creds_fname |>
      populate SSet.empty (fun c accu ->
        return (SSet.add c accu)
      )
    in
    let featured = item.datadir_featured in
    let election = Web_election.make
      raw_election metadata
      ~featured
      ~params_fname
      ~public_keys_fname
    in
    let module X = (val election : WEB_ELECTION) in
    X.B.inject_creds public_creds >>
    return election
  ) index

lwt election_table =
  Lwt_list.fold_left_s (fun accu d ->
    process_datadir d >>=
    wrap1 @@ List.fold_left (fun accu election ->
      let module X = (val election : WEB_ELECTION) in
      let uuid = X.election.e_params.e_uuid in
      EMap.add uuid election accu
    ) accu
  ) EMap.empty !datadirs

let get_election_by_uuid x =
  try_lwt
    EMap.find x election_table |> return
  with Not_found ->
    raise_lwt Eliom_common.Eliom_404

let get_featured_elections () =
  EMap.fold (fun uuid e res ->
    let module X = (val e : WEB_ELECTION) in
    if X.featured then
      e :: res
    else res
  ) election_table [] |> return

module SAuth = Auth_common.Make (struct
  let name = "site"
  let path = []
  let instances = !auth_instances
end)

module SSite = struct
  open Eliom_service

  module Services : SITE_SERVICES = struct
    open Eliom_parameter

    let home = service
      ~path:[]
      ~get_params:unit
      ()

    let source_code = service
      ~path:["belenios.tar.gz"]
      ~get_params:unit
      ()

    let get_randomness = service
      ~path:["get-randomness"]
      ~get_params:unit
      ()

    let saved_service = Eliom_reference.eref
      ~scope:Eliom_common.default_session_scope
      (module struct let s = home end : SAVED_SERVICE)

    let cont () =
      lwt x = Eliom_reference.get saved_service in
      let module X = (val x : SAVED_SERVICE) in
      return X.s

    include SAuth.Services
  end

  module Register (S : SITE_SERVICES) (T : TEMPLATES) : EMPTY = struct
    open Services
    open Eliom_registration

    let () =
      match main_election with
      | None ->
        Html5.register ~service:home
        (fun () () ->
          Eliom_reference.unset saved_service >>
          lwt featured = get_featured_elections () in
          T.home ~featured ()
        )
      | Some uuid ->
        let election = get_election_by_uuid uuid |> Lwt_main.run in
        let module W = (val election : WEB_ELECTION) in
        Redirection.register ~service:home
        (fun () () ->
          Eliom_reference.unset saved_service >>
          return W.S.home
        )

    let () = File.register
      ~service:source_code
      ~content_type:"application/x-gzip"
      (fun () () -> match !source_file with
      | None -> fail_http 404
      | Some f -> return f
      )

    let do_get_randomness =
      let prng = Lazy.lazy_from_fun (Lwt_preemptive.detach (fun () ->
        Cryptokit.Random.(pseudo_rng (string secure_rng 16))
      )) in
      let mutex = Lwt_mutex.create () in
      fun () ->
        Lwt_mutex.with_lock mutex (fun () ->
          lwt prng = Lazy.force prng in
          return Cryptokit.Random.(string prng 32)
        )

    let () = String.register
      ~service:get_randomness
      (fun () () ->
        lwt r = do_get_randomness () in
        Cryptokit.(transform_string (Base64.encode_compact ()) r) |>
        (fun x -> string_of_randomness { randomness=x }) |>
        (fun x -> return (x, "application/json"))
      )

  end

end

module S = SSite.Services
module T = Templates.Make (S)

let () =
  let module X : EMPTY = SAuth.Register (S) (T) in
  let module X : EMPTY = SSite.Register (S) (T) in
  ()

let () =
  EMap.iter (fun _ election ->
    let module W = (val election : WEB_ELECTION) in
    let module X : EMPTY = W.Register (S) (T.Election (W)) in
    ()
  ) election_table
