(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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
open Belenios_api.Serializable_j
open Common

module type PARAMS = sig
  val api_root : string
  val raw_election : string
end

module Make (P : PARAMS) () = struct
  open P

  let () = Stdlib.Random.self_init ()

  module W = Belenios.Election.Make (P) (Random) ()

  module Cred =
    Belenios_core.Credential.Make
      (W.G)
      (struct
        type 'a t = 'a Lwt.t

        let return = Lwt.return
        let bind = Lwt.bind
        let pause = Lwt.pause
        let uuid = W.uuid
        let get_salt _ = Lwt.return_none
      end)

  let nb_candidates =
    match Belenios.Election.compare_version W.witness V1 with
    | None -> failwith "unexpected version"
    | Some Refl -> (
        let question : Belenios_v1.Question.t = W.template.t_questions.(0) in
        let question = Belenios_v1.Question.to_concrete question in
        match question.value with
        | Belenios_question.Homomorphic.Q q -> Array.length q.q_answers
        | _ -> failwith "unexpected question")

  let random_choice () =
    let i = Stdlib.Random.int nb_candidates in
    [| Array.init nb_candidates (fun j -> if j = i then 1 else 0) |]

  let submit_ballot ~username ~ballot =
    let token =
      `Assoc [ ("username", `String username) ]
      |> Yojson.Safe.to_string |> Base64.encode
      |> function
      | Ok x -> x
      | Error (`Msg msg) ->
          Printf.ksprintf failwith "error while encoding to base64: %s" msg
    in
    let headers =
      [ ("Authorization", Printf.sprintf "Bearer %s" token) ]
      |> Cohttp.Header.of_list
    in
    let body = ballot |> Cohttp_lwt.Body.of_string in
    let start = Unix.gettimeofday () in
    let* response, x =
      Cohttp_lwt_unix.Client.post ~headers ~body
        (Printf.ksprintf Uri.of_string "%s/elections/%s/ballots" api_root
           (Uuid.unwrap W.uuid))
    in
    let* () = Cohttp_lwt.Body.drain_body x in
    let delta = Unix.gettimeofday () -. start in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return delta
    | code ->
        Printf.eprintf "unexpected status %d in submit_ballot for %s\n%!" code
          username;
        Lwt.return delta

  let vote ~username ~credential =
    let* x = Cred.derive credential in
    match x with
    | Ok sk ->
        let ballot = W.E.create_ballot ~sk (random_choice ()) in
        let ballot = W.write_ballot -- ballot in
        submit_ballot ~username ~ballot
    | Error _ ->
        Printf.ksprintf failwith "error in deriving key from %s" credential
end

module MakeSlave () = struct
  let main () =
    let open Lwt_io in
    let* api_root = read_line stdin in
    let* raw_election = read_line stdin in
    let module X =
      Make
        (struct
          let api_root = api_root
          let raw_election = raw_election
        end)
        ()
    in
    let rec loop time =
      let* x = read_line_opt stdin in
      match x with
      | None -> Lwt.return time
      | Some x -> (
          match Yojson.Safe.from_string x with
          | `Assoc o -> (
              match
                (List.assoc_opt "username" o, List.assoc_opt "credential" o)
              with
              | Some (`String username), Some (`String credential) ->
                  let* delta = X.vote ~username ~credential in
                  loop (time +. delta)
              | _ -> failwith "unexpected content in JSON")
          | _ | (exception _) -> failwith "unexpected data")
    in
    let* time = loop 0. in
    let* () = write_line stdout (string_of_float time) in
    Lwt.return_unit
end

module type PARAMS_MASTER = sig
  val prefix : string
  val uuid : uuid
  val credentials : string
  val requests : int
  val concurrency : int
end

module MakeMaster (P : PARAMS_MASTER) = struct
  open P

  let api_root = prefix ^ "api"

  let get_raw_election () =
    let* response, x =
      Cohttp_lwt_unix.Client.get
        (Printf.ksprintf Uri.of_string "%s/elections/%s/election" api_root
           (Uuid.unwrap uuid))
    in
    let* x = Cohttp_lwt.Body.to_string x in
    match Cohttp.Code.code_of_status response.status with
    | 200 -> Lwt.return x
    | code ->
        Printf.ksprintf failwith "unexpected status %d in get_raw_election" code

  let submit_ballots_sequential ~credentials ~vote =
    let voters = Array.of_list credentials in
    let nb_voters = Array.length voters in
    let remaining = ref nb_voters in
    let requests = min requests nb_voters in
    let rec loop time n =
      if n > 0 then (
        let i = Stdlib.Random.int !remaining in
        let username, credential = voters.(i) in
        decr remaining;
        voters.(i) <- voters.(!remaining);
        let* delta = vote ~username ~credential in
        loop (time +. delta) (n - 1))
      else Lwt.return time
    in
    loop 0. requests

  let submit_ballots_parallel ~raw_election ~credentials =
    let voters = Array.of_list credentials in
    let nb_voters = Array.length voters in
    let remaining = ref nb_voters in
    let requests = min requests nb_voters in
    let jobs = Array.make concurrency [] in
    let rec loop n t =
      if n > 0 then (
        let i = Stdlib.Random.int !remaining in
        let username, credential = voters.(i) in
        decr remaining;
        voters.(i) <- voters.(!remaining);
        jobs.(t) <- (username, credential) :: jobs.(t);
        loop (n - 1) ((t + 1) mod concurrency))
      else ()
    in
    loop requests 0;
    let jobs = Array.to_list jobs in
    let command =
      (Sys.executable_name, [| Sys.executable_name; "vote"; "--slave" |])
    in
    let* times =
      Lwt_list.map_p
        (fun voters ->
          let p = Lwt_process.open_process command in
          let open Lwt_io in
          let* () = write_line p#stdin api_root in
          let* () = write_line p#stdin raw_election in
          let* () =
            Lwt_list.iter_s
              (fun (username, credential) ->
                `Assoc
                  [
                    ("username", `String username);
                    ("credential", `String credential);
                  ]
                |> Yojson.Safe.to_string |> write_line p#stdin)
              voters
          in
          let* () = close p#stdin in
          let* time = read_line p#stdout in
          let* status = p#close in
          match status with
          | Unix.WEXITED 0 -> Lwt.return (float_of_string time)
          | _ -> failwith "slave exited with non-zero status")
        jobs
    in
    let total_time = List.fold_left ( +. ) 0. times in
    Lwt.return total_time

  let main () =
    let* raw_election = get_raw_election () in
    let module X =
      Make
        (struct
          let api_root = api_root
          let raw_election = raw_election
        end)
        ()
    in
    let* credentials =
      let open Lwt_io in
      let@ f = with_file ~mode:input credentials in
      let* x = Lwt_stream.to_string (read_chars f) in
      match Yojson.Safe.from_string x with
      | `Assoc o ->
          o
          |> List.map (fun (k, v) ->
                 match v with
                 | `String v -> (k, v)
                 | _ -> failwith "unexpected contents in credentials")
          |> Lwt.return
      | _ | (exception _) -> failwith "unexpected JSON in credentials"
    in
    let* time =
      if concurrency < 1 then failwith "concurrency must be >= 1"
      else if concurrency = 1 then
        submit_ballots_sequential ~credentials ~vote:X.vote
      else submit_ballots_parallel ~raw_election ~credentials
    in
    Printf.eprintf "%d requests processed in %f seconds\n" requests time;
    Lwt.return_unit
end

open Cmdliner

let uuid_t =
  let doc = "Use election $(docv)." in
  Arg.(value & opt (some string) None & info [ "uuid" ] ~docv:"UUID" ~doc)

let credentials_t =
  let doc = "Load credentials from file $(docv)." in
  Arg.(
    value
    & opt (some file) None
    & info [ "credentials" ] ~docv:"CREDENTIALS" ~doc)

let requests_t =
  let doc = "Submit $(docv) requests." in
  Arg.(value & opt int 1000 & info [ "requests" ] ~docv:"N" ~doc)

let concurrency_t =
  let doc = "Use $(docv) concurrent threads." in
  Arg.(value & opt int 1 & info [ "concurrency" ] ~docv:"N" ~doc)

let slave_t =
  let doc = "Run in slave mode." in
  Arg.(value & flag & info [ "slave" ] ~doc)

let main slave url uuid credentials requests concurrency =
  let@ () = wrap_main in
  if slave then
    let module X = MakeSlave () in
    Lwt_main.run (X.main ())
  else
    let module X = MakeMaster (struct
      let prefix = get_mandatory "url" url
      let uuid = get_mandatory "uuid" uuid |> Uuid.wrap
      let credentials = get_mandatory "credentials" credentials
      let requests = requests
      let concurrency = concurrency
    end) in
    Lwt_main.run (X.main ())

let cmd =
  let doc = "vote in a scaling test election" in
  let man = [] in
  Cmd.v
    (Cmd.info "vote" ~doc ~man)
    Term.(
      ret
        (const main $ slave_t $ url_t $ uuid_t $ credentials_t $ requests_t
       $ concurrency_t))
