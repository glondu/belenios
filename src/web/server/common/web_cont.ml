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

open Belenios_core.Common
open Web_common

module Make (Web_services : Web_services_sig.S) = struct
  open Web_services
  open Eliom_service
  open Eliom_registration

  let exec cont =
    let redir =
      match cont with
      | { path = ContSiteHome; _ } -> `R (Redirection home)
      | { path = ContSiteAdmin; admin = admin_ui } -> (
          match admin_ui with
          | Classic -> `R (Redirection admin)
          | Basic -> `R (Redirection (admin_basic ()))
          | New -> `R (Redirection (admin_new ())))
      | { path = ContSiteElection uuid; admin = admin_ui } -> (
          match admin_ui with
          | Classic -> `R (Redirection (preapply ~service:election_admin uuid))
          | Basic ->
              let base =
                Eliom_uri.make_string_uri ~service:(admin_basic ())
                  ~absolute:true ()
                |> rewrite_prefix
              in
              `S (Printf.sprintf "%s#elections/%s" base (Uuid.unwrap uuid))
          | New ->
              let base =
                Eliom_uri.make_string_uri ~service:(admin_new ()) ~absolute:true
                  ()
                |> rewrite_prefix
              in
              `S (Printf.sprintf "%s#%s" base (Uuid.unwrap uuid)))
    in
    match redir with
    | `R r -> Redirection.send r
    | `S s -> String_redirection.send s
end
