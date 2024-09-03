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

open Belenios
open Web_common

module Make (Web_services : Web_services_sig.S) = struct
  open Web_services
  open Eliom_registration

  let exec cont =
    let redir =
      match cont with
      | { path = ContSiteHome; admin = admin_ui } -> (
          match admin_ui with
          | Default -> `R (Redirection home)
          | Basic -> `R (Redirection (admin_basic ())))
      | { path = ContSiteElection uuid; admin = admin_ui } -> (
          match admin_ui with
          | Default ->
              let base =
                make_absolute_string_uri ~fragment:(Uuid.unwrap uuid)
                  ~service:(admin_new ()) ()
              in
              `S base
          | Basic ->
              let base =
                make_absolute_string_uri
                  ~fragment:(Printf.sprintf "elections/%s" (Uuid.unwrap uuid))
                  ~service:(admin_basic ()) ()
              in
              `S base)
    in
    match redir with
    | `R r -> Redirection.send r
    | `S s -> String_redirection.send s
end
