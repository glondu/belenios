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

(** Global initialization *)

(* FIXME: the following should be in configuration file... but
   <maxrequestbodysize> doesn't work *)
let () = Ocsigen_config.set_maxrequestbodysizeinmemory 128000

let () = CalendarLib.Time_Zone.(change Local)

(** Parse configuration from <eliom> *)

let data_dirs = ref []
let source_file = ref None
let main_election_uuid = ref None
let auth_instances = ref []

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
    data_dirs := dir :: !data_dirs
  | Element ("rewrite-prefix", ["src", src; "dst", dst], []) ->
    set_rewrite_prefix ~src ~dst
  | Element ("main-election", ["uuid", uuid], []) ->
    (match  Uuidm.of_string uuid with
    | Some u -> main_election_uuid := Some u
    | None -> failwith "Incorrect UUID in configuration <main-election> tag"
    )
  | Element ("auth", ["name", auth_instance],
             [Element (auth_system, auth_config, [])]) ->
    let open Auth_common in
    let i = {auth_system; auth_instance; auth_config} in
    auth_instances := i :: !auth_instances
  | Element (tag, _, _) ->
    Printf.ksprintf failwith
      "invalid configuration for tag %s in belenios"
      tag

(** Parse configuration from other sources *)

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

let ( / ) = Filename.concat

let file_exists x =
  try_lwt
    Lwt_unix.(access x [R_OK]) >>
    return true
  with _ ->
    return false

let parse_election_dir dir =
  Ocsigen_messages.debug (fun () ->
    "Loading data from " ^ dir ^ "..."
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
      "-- loading " ^ subdir
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
    (* public credentials will be parsed later *)
    return Web_election.({
      raw_election;
      metadata;
      featured = item.datadir_featured;
      params_fname;
      public_keys_fname;
    }, public_creds_fname)
  ) index

lwt election_configs =
  Lwt_list.map_p parse_election_dir !data_dirs >>=
  wrap1 List.flatten

(** Build up the site *)

module Site : SITE_SERVICES = struct
  open Eliom_service
  open Eliom_registration

  module Auth = Auth_common.Make (struct
    let name = "site"
    let path = []
    let instances = !auth_instances
  end)

  let main_election = ref None
  let featured = ref []

  (* The following reference is there to cut a dependency loop:
     S.register_election depends on S (via Templates). It will be set
     to a proper value once we have called Templates.Make. *)

  let register_election_ref = ref (fun _ -> assert false)

  (* We use an intermediate module S that will be passed to Templates
     and Web_election. S is not meant to leak and will be included
     in the returned module later. *)

  module S : SITE_SERVICES = struct
    include Auth.Services
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

    let register_election config = !register_election_ref config

    let cont () =
      lwt x = Eliom_reference.get saved_service in
      let module X = (val x : SAVED_SERVICE) in
      return X.s

  end

  include S

  module T = Templates.Make (S)

  let () = register_election_ref := fun config ->
    let registration = Web_election.make config in
    let module R = (val registration : Web_election.REGISTRATION) in
    let module W = R.W in
    let module X : EMPTY = R.Register (S) (T.Election (W)) in
    let election = (module W : WEB_ELECTION) in
    let u = W.election.e_params.e_uuid in
    if !main_election_uuid = Some u then main_election := Some election;
    if W.featured then featured := election :: !featured;
    return election

  let () = let module X : EMPTY = Auth.Register (S) (T) in ()

  let () = Any.register ~service:home
    (fun () () ->
      Eliom_reference.unset saved_service >>
      match !main_election with
      | None ->
        T.home ~featured:!featured () >>= Html5.send
      | Some w ->
        let module W = (val w : WEB_ELECTION) in
        Redirection.send W.S.home
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



let populate accu f s = Lwt_stream.fold_s f s accu

lwt () =
  Lwt_list.iter_s (fun (config, public_creds_fname) ->
    lwt election = Site.register_election config in
    let module W = (val election : WEB_ELECTION) in
    lwt public_creds =
      Lwt_io.lines_of_file public_creds_fname |>
      populate SSet.empty (fun c accu ->
        return (SSet.add c accu)
      )
    in
    W.B.inject_creds public_creds >>
    return ()
  ) election_configs
