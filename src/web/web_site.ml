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
open Util
open Serializable_t
open Signatures
open Web_serializable_j
open Web_common
open Web_signatures

module type CONFIG = sig
  val name : string
  val path : string list
  val source_file : string
  val auth_config : auth_config list
end

module Make (C : CONFIG) : SITE = struct
  open Eliom_service
  open Eliom_registration

  let make_path x = C.path @ x

  module Auth = Web_auth.Make (C)

  let main_election = ref None
  let featured = ref []

  (* The following reference is there to cut a dependency loop:
     S.register_election depends on S (via Templates). It will be set
     to a proper value once we have called Templates.Make. *)

  let register_election_ref = ref (fun _ -> assert false)

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

    let register_election config = !register_election_ref config

    let set_main_election x = main_election := Some x
    let unset_main_election () = main_election := None

  end

  include S

  module T = Web_templates.Make (S)

  let () = register_election_ref := fun config ->
    let registration = Web_election.make config in
    let module R = (val registration : Web_election.REGISTRATION) in
    let module W = R.W in
    let module X : EMPTY = R.Register (S) (T) in
    let election = (module W : WEB_ELECTION) in
    let election_ro = (module W : WEB_ELECTION_RO) in
    if W.featured then featured := election_ro :: !featured;
    return election

  let () = let module X : EMPTY = Auth.Register (S) (T.Login (S)) in ()

  let () = Any.register ~service:home
    (fun () () ->
      Eliom_reference.unset cont >>
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
