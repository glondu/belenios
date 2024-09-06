(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2022 Inria                                           *)
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

module Make () = struct
  let belenios_scope = `Session (Eliom_common.create_scope_hierarchy "belenios")
  let site_user = Eliom_reference.eref ~scope:belenios_scope None
  let cast_confirmed = Eliom_reference.eref ~scope:belenios_scope None
  let signup_address = Eliom_reference.eref ~scope:belenios_scope None
  let signup_env = Eliom_reference.eref ~scope:belenios_scope None
  let set_email_env = Eliom_reference.eref ~scope:belenios_scope None
  let billing_env = Eliom_reference.eref ~scope:belenios_scope None
  let discard () = Eliom_state.discard ~scope:belenios_scope ()

  let get_consent_cookie () =
    let cookies = Eliom_request_info.get_cookies () in
    match Ocsigen_cookie_map.Map_inner.find_opt "belenios-consent" cookies with
    | None -> true
    | Some x -> (
        match float_of_string x with
        | exception _ -> true
        | x -> x < !Web_config.tos_last_update)

  let set_consent_cookie () =
    let now = Unix.gettimeofday () in
    let exp = now +. (10. *. 365. *. 86400.) in
    Eliom_state.set_cookie ~exp ~name:"belenios-consent"
      ~value:(string_of_float now) ()
end
