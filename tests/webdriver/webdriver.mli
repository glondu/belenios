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

type session
type element
type window

val json_of_element : element -> Yojson.Safe.t

class webdriver : session -> object
  method implicit_wait : unit Lwt.t
  method navigate_to : string -> unit Lwt.t
  method get_url : string Lwt.t
  method get_elements : selector:string -> element list Lwt.t
  method get_sub_elements : element -> selector:string -> element list Lwt.t
  method accept : unit Lwt.t
  method click : element -> unit Lwt.t
  method clear : element -> unit Lwt.t
  method send_keys : element -> string -> unit Lwt.t
  method get_windows : window list Lwt.t
  method switch_to_window : window -> unit Lwt.t
  method maximize_window : unit Lwt.t

  method set_window_rect :
    ?width:int -> ?height:int -> ?x:int -> ?y:int -> unit -> unit Lwt.t

  method close_window : unit Lwt.t

  method execute :
    script:string -> args:Yojson.Safe.t list -> Yojson.Safe.t option Lwt.t
end

class helpers : session -> object
  inherit webdriver

  method with_single_element :
    'a. selector:string -> unit -> (element -> 'a Lwt.t) -> 'a Lwt.t

  method fill_with : selector:string -> string -> unit Lwt.t
  method click_on : selector:string -> unit Lwt.t
  method click_on_last_button : unit Lwt.t
end

val with_session :
  ?implicit_timeout:int ->
  headless:bool ->
  url:string ->
  unit ->
  (session -> 'a Lwt.t) ->
  'a Lwt.t
