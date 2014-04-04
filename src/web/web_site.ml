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

open Lwt
open Serializable_j
open Signatures
open Common
open Web_serializable_j
open Web_common
open Web_signatures

module type CONFIG = sig
  val name : string
  val path : string list
  val source_file : string
  val spool_dir : string
  val auth_config : auth_config list
end

let rec list_remove x = function
  | [] -> []
  | y :: ys -> if x = y then ys else y :: (list_remove x ys)

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

module Make (C : CONFIG) : SITE = struct
  open Eliom_service
  open Eliom_registration

  module C = struct
    include C
    let kind = `Site
  end

  let make_path x = C.path @ x

  module Auth = Web_auth.Make (C)
  module Random = MakeLwtRandom (struct let rng = make_rng () end)

  let store = Ocsipersist.open_store C.name

  (* Persistent table, used to initialize the server. *)
  let election_ptable = Ocsipersist.open_table (C.name ^ "_elections")

  (* In-memory table, indexed by UUID, contains closures. *)
  let election_table = ref SMap.empty

  lwt main_election =
    Ocsipersist.make_persistent store "main_election" None

  lwt featured =
    Ocsipersist.make_persistent store "featured_elections" []

  (* The following reference is there to cut a dependency loop:
     S.register_election depends on S (via Templates). It will be set
     to a proper value once we have called Templates.Make. *)

  let import_election_ref = ref (fun _ -> assert false)

  (* We use an intermediate module S that will be passed to Templates
     and Web_election. S is not meant to leak and will be included
     in the returned module later. *)

  module S : SITE = struct
    include Auth.Services
    include Auth.Handlers
    open Eliom_parameter

    let scope = Eliom_common.default_session_scope

    let home = service
      ~path:(make_path [""])
      ~get_params:unit
      ()

    let admin = service
      ~path:(make_path ["admin"])
      ~get_params:unit
      ()

    let source_code = service
      ~path:(make_path ["belenios.tar.gz"])
      ~get_params:unit
      ()

    let get_randomness = service
      ~path:(make_path ["get-randomness"])
      ~get_params:unit
      ()

    let cont = Eliom_reference.eref ~scope
      (fun () () -> Eliom_registration.Redirection.send home)

    let import_election f = !import_election_ref f

    let add_featured_election x =
      lwt the_featured = Ocsipersist.get featured in
      if List.mem x the_featured then (
        return ()
      ) else if SMap.mem x !election_table then (
        Ocsipersist.set featured (x :: the_featured)
      ) else (
        Lwt.fail Not_found
      )

    let remove_featured_election x =
      lwt the_featured = Ocsipersist.get featured in
      Ocsipersist.set featured (list_remove x the_featured)

    let set_main_election x =
      if SMap.mem x !election_table then (
        Ocsipersist.set main_election (Some x)
      ) else (
        Lwt.fail Not_found
      )

    let unset_main_election () =
      Ocsipersist.set main_election None

  end

  include S

  module T = Web_templates.Make (S)

  let register_election params web_params =
    let module P = (val params : ELECTION_PARAMS) in
    let uuid = Uuidm.to_string P.params.e_uuid in
    let module D = struct
      module G = P.G
      let election = {
        e_params = P.params;
        e_pks = None;
        e_fingerprint = P.fingerprint;
      }
    end in
    let module P = (val web_params : WEB_PARAMS) in
    let module R = Web_election.Make (D) (P) in
    (module R : Web_election.REGISTRABLE), fun () ->
      (* starting from here, we do side-effects on the running server *)
      let module R = R.Register (struct end) in
      let module W = R.W in
      let module X : EMPTY = R.Register (S) (T) in
      let election = (module W : WEB_ELECTION) in
      election_table := SMap.add uuid election !election_table;
      election

  (* Mutex to avoid simultaneous registrations of the same election *)
  let registration_mutex = Lwt_mutex.create ()

  let () = import_election_ref := fun f ->
    Lwt_mutex.lock registration_mutex >>
    try_lwt
      lwt raw_election =
        Lwt_io.lines_of_file f.f_election |>
        get_single_line >>=
        (function
        | Some e -> return e
        | None -> Printf.ksprintf
          failwith "%s must contain a single line" f.f_election
        )
      in
      let params = Group.election_params_of_string raw_election in
      let module P = (val params : ELECTION_PARAMS) in
      let uuid = Uuidm.to_string P.params.e_uuid in
      lwt exists =
        try_lwt
          lwt _ = Ocsipersist.find election_ptable uuid in
          return true
        with Not_found -> return false
      in
      if exists then (
        return None
      ) else (
        let ( / ) = Filename.concat in
        let dir = C.spool_dir/uuid in
        lwt metadata =
          Lwt_io.chars_of_file f.f_metadata |>
          Lwt_stream.to_string >>=
          wrap1 metadata_of_string
        in
        let module X = struct
          let metadata = metadata
          let dir = dir
        end in
        let web_params = (module X : WEB_PARAMS) in
        let r, do_register = register_election params web_params in
        let module R = (val r : Web_election.REGISTRABLE) in
        let module G = R.W.G in
        let module KG = Election.MakeSimpleDistKeyGen (G) (Random) in
        let public_keys = Lwt_io.lines_of_file f.f_public_keys in
        lwt pks = Lwt_stream.(
          clone public_keys |>
          map (trustee_public_key_of_string G.read) |>
          to_list >>= wrap1 Array.of_list
        ) in
        if not (Array.forall KG.check pks) then
          failwith "Public keys are invalid.";
        if not G.(R.W.election.e_params.e_public_key =~ KG.combine pks) then
          failwith "Public keys mismatch with election public key.";
        let public_creds = Lwt_io.lines_of_file f.f_public_creds in
        lwt () = Lwt_stream.(
          clone public_creds |>
          iter_s (fun x ->
            if not G.(check @@ of_string x) then (
              Lwt.fail @@ Failure "Public credentials are invalid."
            ) else return ()
          )
        ) in
        let module R = struct
          let discard () = Lwt_mutex.unlock registration_mutex
          let register () =
            if not (Lwt_mutex.is_locked registration_mutex) then
              failwith "This election can no longer be registered.";
            try_lwt
              Lwt_unix.mkdir dir 0o700 >>
              Lwt_io.(with_file Output (dir/"election.json") (fun oc ->
                write_line oc raw_election
              )) >>
              Lwt_io.(with_file Output (dir/"public_keys.jsons") (fun oc ->
                write_lines oc public_keys
              )) >>
              let election = do_register () in
              let module W = (val election : WEB_ELECTION) in
              let () =
                Ocsigen_messages.debug (fun () ->
                  Printf.sprintf "Injecting credentials for %s" uuid
                )
              in
              public_creds |>
              Lwt_stream.iter_s W.B.inject_cred >>
              W.B.update_files () >>
              Ocsipersist.add election_ptable uuid (raw_election, web_params) >>
              let () = Lwt_mutex.unlock registration_mutex in
              return election
            with e ->
              Lwt_mutex.unlock registration_mutex;
              Lwt.fail e
        end in
        (* until here, no side-effects on the running server *)
        return @@ Some (module R : REGISTRABLE_ELECTION)
      )
    with e ->
      Lwt_mutex.unlock registration_mutex;
      Lwt.fail e

  lwt () =
    Ocsipersist.iter_step (fun uuid (raw_election, web_params) ->
      let params = Group.election_params_of_string raw_election in
      let _, do_register = register_election params web_params in
      let election = do_register () in
      let module W = (val election : WEB_ELECTION) in
      assert (uuid = Uuidm.to_string W.election.e_params.e_uuid);
      Ocsigen_messages.debug (fun () ->
        Printf.sprintf "Initialized election %s from persistent store" uuid
      );
      return ()
    ) election_ptable

  let () = let module X : EMPTY = Auth.Register (S) (T.Login (S)) in ()

  let () = Any.register ~service:home
    (fun () () ->
      Eliom_reference.unset cont >>
      match_lwt Ocsipersist.get main_election with
      | None ->
        lwt featured =
          Ocsipersist.get featured >>=
          Lwt_list.map_p (fun x ->
            let module W = (val SMap.find x !election_table : WEB_ELECTION) in
            return (module W : WEB_ELECTION_RO)
          )
        in
        T.home ~featured () >>= Html5.send
      | Some x ->
        let module W = (val SMap.find x !election_table : WEB_ELECTION) in
        Redirection.send W.S.home
    )

  let () = Html5.register ~service:admin
    (fun () () ->
      let cont () () = Redirection.send admin in
      Eliom_reference.set S.cont cont >>
      lwt elections =
        match_lwt get_user () with
        | None -> return []
        | Some u ->
          SMap.fold (fun _ w accu ->
            let module W = (val w : WEB_ELECTION) in
            if W.metadata.e_owner = Some u then (
              (module W : WEB_ELECTION_RO) :: accu
            ) else (
              accu
            )
          ) !election_table [] |> List.rev |> return
      in
      T.admin ~elections ()
    )

  let () = File.register
    ~service:source_code
    ~content_type:"application/x-gzip"
    (fun () () -> return C.source_file)

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
