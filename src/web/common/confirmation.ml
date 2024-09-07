(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2024-2024 Inria                                           *)
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

open Belenios_api.Serializable_t

type error = ElectionClosed | UnauthorizedVoter | CastError of cast_error

let explain_error l e =
  let open (val l : I18n.GETTEXT) in
  match e with
  | ElectionClosed -> s_ "the election is closed"
  | UnauthorizedVoter -> s_ "you are not allowed to vote"
  | CastError (`SerializationError e) ->
      Printf.sprintf (f_ "your ballot has a syntax error (%s)") e
  | CastError `NonCanonical -> s_ "your ballot is not in canonical form"
  | CastError `InvalidBallot -> s_ "some proofs failed verification"
  | CastError `InvalidCredential -> s_ "your credential is invalid"
  | CastError `RevoteNotAllowed -> s_ "you are not allowed to revote"
  | CastError `UsedCredential -> s_ "your credential has already been used"
  | CastError `WrongCredential ->
      s_ "you are not allowed to vote with this credential"
  | CastError `WrongWeight -> s_ "your credential has a bad weight"
  | CastError `DuplicateBallot -> s_ "this ballot has already been accepted"
  | CastError `ExpiredBallot -> s_ "this ballot has expired"
  | CastError `WrongUsername -> s_ "your username is wrong"
