(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2012-2023 Inria                                           *)
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

open Lwt.Syntax
open Belenios
open Web_common
open Eliom_content.Html.F

module Make
    (Web_state : Web_state_sig.S)
    (Web_i18n : Web_i18n_sig.S)
    (Web_services : Web_services_sig.S)
    (Pages_common : Pages_common_sig.S) =
struct
  open Web_services
  open Pages_common

  let get_preferred_gettext () = Web_i18n.get_preferred_gettext "voter"

  let lost_ballot s election () =
    let* l = get_preferred_gettext () in
    let open (val l) in
    let open (val election : Site_common_sig.ELECTION) in
    let title = template.t_name in
    let full_title = title in
    let* metadata = Web_persist.get_election_metadata s uuid in
    let you_must_restart =
      match get_booth_index metadata.e_booth_version with
      | Some i ->
          let (Booth service) = fst Web_services.booths.(i) in
          let hash =
            Netencoding.Url.mk_url_encoded_parameters
              [ ("uuid", Uuid.unwrap uuid) ]
          in
          div
            [
              txt (s_ "If you want to vote, you must ");
              make_a_with_hash ~service:(service ()) ~hash
                (s_ "start from the beginning");
              txt ".";
            ]
      | None -> txt ""
    in
    let content =
      [
        div
          [
            b [ txt (s_ "Warning:") ];
            txt " ";
            txt (s_ "Your vote was not recorded!");
          ];
        you_must_restart;
        div
          [
            a ~service:Web_services.election_home_redirect
              [ txt (s_ "Go back to election") ]
              (uuid, ());
          ];
      ]
    in
    base ~full_title ~title ~content ~uuid ()
end
