(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
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
open Belenios_web_api
open Belenios_storage_api
open Web_common
open Api_generic

let with_administrator (token : token_user) f =
  match token.token with
  | None -> (
      match token.user with None -> unauthorized | Some (_, a, _) -> f a)
  | Some token -> (
      match lookup_token token with Some (a, _) -> f a | _ -> unauthorized)

let with_administrator_or_nobody (token : token_user)
    (metadata : trustees_metadata) f =
  match token.token with
  | None -> f `Nobody
  | Some token -> (
      match lookup_token token with
      | Some (a, _) when Accounts.check a metadata.owners ->
          f (`Administrator a)
      | _ -> not_found)

let with_trustee s w (token : token_user) (metadata : trustees_metadata) f =
  let@ token = Option.unwrap unauthorized token.token in
  match metadata.trustees with
  | None -> (
      let@ dt, set = Storage.T.update s (Trustees_draft w) in
      let set x = set Value x in
      match Lopt.get_value dt with
      | None -> unauthorized
      | Some dt -> (
          let i =
            match dt.mode with
            | `Basic p ->
                List.find_index
                  (fun (t : _ draft_basic_trustee) -> t.token = token)
                  p.trustees
            | `Threshold p ->
                List.find_index
                  (fun (t : _ draft_threshold_trustee) -> t.token = token)
                  p.trustees
          in
          match i with
          | Some i -> f (token, i + 1, Some (dt, set))
          | None -> unauthorized))
  | Some ts -> (
      match
        List.find_index (fun (t : external_trustee) -> t.token = token) ts
      with
      | Some i -> f (token, i + 1, None)
      | None -> unauthorized)

let get_draft_trustees ~is_admin (trustees : _ draft_trustees) :
    _ Belenios_web_api.draft_trustees =
  let mode =
    match trustees.mode with
    | `Basic x ->
        let trustees =
          List.map
            (fun (t : _ draft_basic_trustee) ->
              let state, key =
                match t.parameters with
                | None -> (Some 0, None)
                | Some tpk -> (Some 1, Some tpk)
              in
              let address, token, state =
                if is_admin then (Some t.address, Some t.token, state)
                else (None, None, None)
              in
              { address; name = t.name; token; state; key })
            x.trustees
        in
        `Basic ({ trustees } : _ basic_trustees)
    | `Threshold x ->
        let trustees =
          List.map
            (fun (t : _ draft_threshold_trustee) ->
              let address, token, state =
                if is_admin then
                  ( Some t.address,
                    Some t.token,
                    Some (Option.value t.step ~default:0) )
                else (None, None, None)
              in
              { address; name = t.name; token; state; key = t.cert })
            x.trustees
        in
        `Threshold
          ({ threshold = x.threshold; trustees } : _ threshold_trustees)
  in
  { step = trustees.step; mode }

let check_address address =
  if not @@ is_email address then raise (Error (`Invalid "e-mail address"))

let post_draft_trustees ((dt, set) : _ draft_trustees updatable)
    ({ address; name } : addable_trustee) =
  check_address address;
  match dt.mode with
  | `Basic x ->
      let ts = x.trustees in
      let () =
        if
          List.exists
            (fun (x : _ draft_basic_trustee) -> x.address = address)
            ts
        then raise (Error (`GenericError "address already used"))
      in
      let token = generate_token 22 in
      x.trustees <- ts @ [ { address; name; token; parameters = None } ];
      set dt
  | `Threshold x ->
      let ts = x.trustees in
      let () =
        if
          List.exists
            (fun (x : _ draft_threshold_trustee) -> x.address = address)
            ts
        then raise (Error (`GenericError "address already used"))
      in
      let token = generate_token 22 in
      let t =
        {
          address;
          name;
          token;
          step = None;
          cert = None;
          polynomial = None;
          vinput = None;
          voutput = None;
        }
      in
      x.trustees <- ts @ [ t ];
      set dt

let set_threshold ((dt, set) : _ draft_trustees updatable) threshold =
  match dt.mode with
  | `Basic _ -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x when x.trustees = [] -> Lwt.return @@ Stdlib.Error `NoTrustees
  | `Threshold x ->
      let ts = x.trustees in
      let maybe_threshold, step =
        if threshold = 0 then (None, None) else (Some threshold, Some 1)
      in
      if 0 <= threshold && threshold < List.length ts then (
        List.iter (fun (t : _ draft_threshold_trustee) -> t.step <- step) ts;
        x.threshold <- maybe_threshold;
        let* () = set dt in
        Lwt.return @@ Ok ())
      else Lwt.return @@ Stdlib.Error `OutOfBounds

let get_draft_trustees_mode (dt : _ draft_trustees) =
  match dt.mode with
  | `Basic _ -> `Basic
  | `Threshold x -> `Threshold (Option.value x.threshold ~default:0)

let put_draft_trustees_mode ((dt, set) : _ draft_trustees updatable) mode =
  match (get_draft_trustees_mode dt, mode) with
  | a, b when a = b -> Lwt.return_unit
  | _, `Basic ->
      let dt = { step = 1; mode = `Basic { trustees = [] } } in
      set dt
  | `Basic, `Threshold 0 ->
      let dtp =
        {
          algorithm = default_algorithm;
          threshold = None;
          trustees = [];
          parameters = None;
          error = None;
        }
      in
      let dt = { step = 1; mode = `Threshold dtp } in
      set dt
  | `Threshold _, `Threshold threshold -> (
      let* x = set_threshold (dt, set) threshold in
      match x with
      | Ok () -> Lwt.return_unit
      | Error `NoTrustees -> Lwt.fail (Error (`GenericError "no trustees"))
      | Error `OutOfBounds ->
          Lwt.fail (Error (`GenericError "threshold out of bounds")))
  | _, _ -> Lwt.fail (Error (`GenericError "change not allowed"))

let reset_draft_trustees ((_, set) : _ draft_trustees updatable) =
  let dt = { step = 1; mode = `Basic { trustees = [] } } in
  set dt

let rec filter_out_first f = function
  | [] -> (false, [])
  | x :: xs ->
      if f x then (true, xs)
      else
        let touched, xs = filter_out_first f xs in
        (touched, x :: xs)

let delete_draft_trustee ((dt, set) : _ draft_trustees updatable) trustee =
  match dt.mode with
  | `Basic x ->
      let ts = x.trustees in
      let touched, ts =
        filter_out_first
          (fun (x : _ draft_basic_trustee) -> x.address = trustee)
          ts
      in
      if touched then (
        x.trustees <- ts;
        let* () = set dt in
        Lwt.return_true)
      else Lwt.return_false
  | `Threshold x ->
      let ts = x.trustees in
      let touched, ts =
        filter_out_first
          (fun (x : _ draft_threshold_trustee) -> x.address = trustee)
          ts
      in
      if touched then (
        x.trustees <- ts;
        let* () = set dt in
        Lwt.return_true)
      else Lwt.return_false

let post_trustee_basic (type a b) (w : (a, b) group)
    ((dt, set) : _ draft_trustees updatable) ~token (p : _ draft_basic_params)
    data =
  let module G = (val w) in
  let ts = p.trustees in
  let t =
    match
      List.find_opt (fun (x : _ draft_basic_trustee) -> x.token = token) ts
    with
    | Some t -> t
    | None -> failwith "Invalid token"
  in
  match t.parameters with
  | None ->
      let module T = (val Trustees.get_by_version G.spec.version) in
      let parameters = [%group_of_yojson: _ basic_parameters] data in
      let module K = T.MakeCombinator (G) in
      if
        parameters.verification_key.message.message.name = Some t.name
        && K.check [ `Single parameters ]
      then (
        t.parameters <- Some parameters;
        set dt)
      else raise @@ Error `InvalidPublicKey
  | _ -> raise @@ Error `PublicKeyExists

let post_trustee_threshold (type a b) (w : (a, b) group)
    ((dt, set) : (a, b) draft_trustees updatable) ~token
    (dtp : _ draft_threshold_params) data =
  let module G = (val w) in
  let ts = Array.of_list dtp.trustees in
  let threshold =
    match dtp.threshold with Some t -> t | None -> failwith "No threshold set"
  in
  let i, t =
    match
      Array.findi
        (fun i (x : _ draft_threshold_trustee) ->
          if token = x.token then Some (i, x) else None)
        ts
    with
    | Some (i, t) -> (i, t)
    | None -> failwith "Trustee not found"
  in
  let names = Array.map (fun (x : _ draft_threshold_trustee) -> x.name) ts in
  let context =
    { algorithm = dtp.algorithm; group = G.spec.group; names; threshold }
  in
  let full_context = { context; index = i + 1 } in
  let get_certs () =
    Array.map
      (fun (x : _ draft_threshold_trustee) ->
        match x.cert with None -> failwith "Missing certificate" | Some y -> y)
      ts
  in
  let get_polynomials () =
    Array.map
      (fun (x : _ draft_threshold_trustee) ->
        match x.polynomial with
        | None -> failwith "Missing polynomial"
        | Some y -> y)
      ts
  in
  let module T = (val Trustees.get_by_version G.spec.version) in
  let module P = Pki.Make (G) in
  let module C = Pki.MakeChannels (P) in
  let module K = T.MakePedersen (C) in
  let () =
    match t.step with
    | Some 1 ->
        let cert = [%group_of_yojson: _ pedersen_cert] data in
        if K.step1_check full_context cert then (
          t.cert <- Some cert;
          t.step <- Some 2)
        else failwith "Invalid certificate"
    | Some 3 ->
        let certs = get_certs () in
        let polynomial = [%group_of_yojson: _ polynomial] data in
        if K.step3_check { context; certs } i polynomial then (
          t.polynomial <- Some polynomial;
          t.step <- Some 4)
        else failwith "Invalid polynomial"
    | Some 5 ->
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutput = [%group_of_yojson: _ voutput] data in
        if K.step5_check { context; certs } i polynomials voutput then (
          t.voutput <- Some voutput;
          t.step <- Some 6)
        else failwith "Invalid voutput"
    | _ -> failwith "Invalid step"
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 2) ts
    then
      try
        let certs = { context; certs = get_certs () } in
        let threshold = K.step2 certs in
        assert (dtp.threshold = Some threshold);
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 3) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 4) ts
    then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let vinputs = K.step4 { context; certs } polynomials in
        for j = 0 to Array.length ts - 1 do
          ts.(j).vinput <- Some vinputs.(j)
        done;
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 5) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  let () =
    if Array.for_all (fun (x : _ draft_threshold_trustee) -> x.step = Some 6) ts
    then
      try
        let certs = get_certs () in
        let polynomials = get_polynomials () in
        let voutputs =
          Array.map
            (fun x ->
              match x.voutput with
              | None -> failwith "Missing voutput"
              | Some y -> y)
            ts
        in
        let p = K.step6 { context; certs } polynomials voutputs in
        dtp.parameters <- Some p;
        Array.iter (fun (x : _ draft_threshold_trustee) -> x.step <- Some 7) ts
      with e -> dtp.error <- Some (Printexc.to_string e)
  in
  set dt

let post_trustee w ((dt, set) : _ draft_trustees updatable) ~token data =
  match dt.mode with
  | `Basic p -> post_trustee_basic w (dt, set) ~token p data
  | `Threshold p -> post_trustee_threshold w (dt, set) ~token p data

let validation_error x = raise (Api_generic.Error (`ValidationError x))

let validate s (type a b) (w : (a, b) group) (dt : (a, b) draft_trustees)
    ((metadata, set_metadata) : trustees_metadata updatable) =
  let module G = (val w) in
  let names, trustees, private_keys =
    match dt.mode with
    | `Basic x ->
        let ts = x.trustees in
        ( List.map
            (fun ({ address; token; _ } : _ draft_basic_trustee) ->
              ({ address; token } : external_trustee))
            ts,
          List.map
            (fun (x : _ draft_basic_trustee) ->
              match x.parameters with
              | None -> validation_error `KeyEstablishmentNotFinished
              | Some x -> `Single x)
            ts,
          None )
    | `Threshold x -> (
        let ts = x.trustees in
        match x.parameters with
        | None -> validation_error `KeyEstablishmentNotFinished
        | Some tp ->
            ( List.map
                (fun ({ address; token; _ } : _ draft_threshold_trustee) ->
                  ({ address; token } : external_trustee))
                ts,
              [ `Pedersen tp ],
              Some
                (List.map
                   (fun { voutput; _ } ->
                     match voutput with
                     | Some v -> v.private_key
                     | None -> failwith "inconsistent state")
                   ts) ))
  in
  let* () = Storage.T.set s (Trustees G.spec) Value trustees in
  let* () =
    match private_keys with
    | None -> Lwt.return_unit
    | Some x -> Storage.T.set s (Trustees_private_keys G.spec) Value x
  in
  let* () = set_metadata { metadata with trustees = Some names } in
  Storage.T.del s (Trustees_draft G.spec)

let get_synthetic_draft_trustees is_admin (metadata : trustees_metadata)
    (trustees : _ trustees) : _ Belenios_web_api.draft_trustees option =
  let open Belenios_web_api in
  let@ external_trustees cont =
    match metadata.trustees with None -> None | Some x -> cont x
  in
  let@ mode (cont : _ draft_trustees_mode -> _) =
    match trustees with
    | [ `Pedersen p ] ->
        Array.combine
          (Array.of_list external_trustees)
          (Array.combine p.certs p.verification_keys)
        |> Array.map
             (fun
               ( (t : external_trustee),
                 (cert, (vk : _ threshold_verification_key)) )
             ->
               let name = vk.message.message.name in
               let name = Option.value ~default:"N/A" name in
               let token, address =
                 if is_admin then (Some t.token, Some t.address)
                 else (None, None)
               in
               ({ name; address; token; key = Some cert; state = Some 7 }
                 : _ pedersen_cert trustee))
        |> Array.to_list
        |> fun trustees ->
        cont
        @@ `Threshold
             ({ threshold = Some p.context.threshold; trustees }
               : _ threshold_trustees)
    | _ -> (
        try
          List.combine external_trustees trustees
          |> List.map (fun ((t : external_trustee), t') ->
              match t' with
              | `Pedersen _ -> raise Exit
              | `Single (p : _ basic_parameters) ->
                  let name = p.verification_key.message.message.name in
                  let name = Option.value ~default:"N/A" name in
                  let token, address =
                    if is_admin then (Some t.token, Some t.address)
                    else (None, None)
                  in
                  ({ name; address; token; key = Some p; state = Some 1 }
                    : _ basic_parameters trustee))
          |> fun trustees -> cont @@ `Basic { trustees }
        with Exit -> None)
  in
  Some { step = 3; mode }

let dispatch_trustees ~token ~ifmatch endpoint method_ body s
    ((metadata, set_metadata) : _ updatable) (type a b) (w : (a, b) group) =
  let module G = (val w) in
  match endpoint with
  | [] -> (
      let* x = Storage.T.get s (Trustees G.spec) in
      match Lopt.get_value x with
      | None -> not_found
      | Some x -> return_yojson 200 @@ [%yojson_of_group: _ trustees] x)
  | [ "group" ] ->
      let { version; group; _ } : trustees_metadata = metadata in
      return_yojson 200 @@ yojson_of_group_specification { version; group }
  | [ "draft" ] -> (
      let@ who = with_administrator_or_nobody token metadata in
      let@ dt =
       fun cont ->
        let@ dt, set = Storage.T.update s (Trustees_draft G.spec) in
        match Lopt.get_value dt with
        | None -> cont None
        | Some dt -> cont @@ Some (dt, fun x -> set Value x)
      in
      let get dt is_admin () =
        let open Belenios_web_api in
        let x = get_draft_trustees ~is_admin dt in
        Lwt.return @@ [%yojson_of_group: _ draft_trustees] x
      in
      let get_synthetic is_admin =
        let open Belenios_web_api in
        let* x = Storage.T.get s (Trustees G.spec) in
        match Lopt.get_value x with
        | None -> Lwt.return_none
        | Some trustees -> (
            let x = get_synthetic_draft_trustees is_admin metadata trustees in
            match x with
            | None -> Lwt.return_none
            | Some x ->
                Lwt.return_some @@ [%yojson_of_group: _ draft_trustees] x)
      in
      match (method_, who, dt) with
      | `GET, _, None -> (
          let* x =
            get_synthetic
              (match who with `Administrator _ -> true | _ -> false)
          in
          match x with None -> not_found | Some x -> return_yojson 200 x)
      | `GET, `Nobody, Some (dt, _) -> handle_get (get dt false)
      | `GET, `Administrator _, Some (dt, _) -> handle_get (get dt true)
      | `POST, `Administrator _, Some (dt, set) -> (
          let@ () = handle_ifmatch ifmatch (get dt true) in
          let@ () =
           fun cont -> if metadata.trustees <> None then forbidden else cont ()
          in
          let@ request = body.run !*trustees_request_of_yojson in
          let@ () = handle_generic_error in
          match request with
          | `SetStep step ->
              let* () =
                if step <> dt.step then set { dt with step }
                else Lwt.return_unit
              in
              ok
          | `AddTrustee trustee ->
              let* () = post_draft_trustees (dt, set) trustee in
              ok
          | `RemoveTrustee trustee ->
              let* x = delete_draft_trustee (dt, set) trustee in
              if x then ok else not_found
          | `SetBasic ->
              let* () = put_draft_trustees_mode (dt, set) `Basic in
              ok
          | `SetThreshold t ->
              let* () = put_draft_trustees_mode (dt, set) (`Threshold t) in
              ok
          | `Validate ->
              let* () = validate s w dt (metadata, set_metadata) in
              ok
          | `Reset ->
              let* () = reset_draft_trustees (dt, set) in
              ok)
      | _ -> method_not_allowed)
  | [ "trustee" ] -> (
      let@ token, index, dt = with_trustee s G.spec token metadata in
      match method_ with
      | `GET -> (
          let@ () =
           fun cont ->
            let* x = cont () in
            return_yojson 200 @@ [%yojson_of_group: _ trustees_trustee_status] x
          in
          let@ dt cont =
            match dt with
            | None ->
                let* uuids = Storage.T.get_elections s in
                Lwt.return @@ `Ready uuids
            | Some (dt, _) -> cont dt
          in
          let@ () = fun cont -> Lwt.return @@ `Draft (cont ()) in
          match dt.mode with
          | `Basic p -> (
              let@ () = fun cont -> `Basic (cont ()) in
              let t = List.nth p.trustees (index - 1) in
              match t.parameters with
              | None -> `Init t.name
              | Some p -> `Done p.cert.message.verification)
          | `Threshold p -> (
              let@ () = fun cont -> `Threshold (cont ()) in
              let@ threshold cont =
                match p.threshold with None -> `Init | Some x -> cont x
              in
              let ts = Array.of_list p.trustees in
              let t = ts.(index - 1) in
              let names =
                ts |> Array.map (fun (x : _ draft_threshold_trustee) -> x.name)
              in
              let context =
                {
                  algorithm = p.algorithm;
                  group = G.spec.group;
                  names;
                  threshold;
                }
              in
              let context = { context; index } in
              match t.cert with
              | None -> `WaitingForCertificate context
              | Some cert -> (
                  try
                    let certs =
                      ts
                      |> Array.map (fun (x : _ draft_threshold_trustee) ->
                          match x.cert with None -> raise Exit | Some c -> c)
                    in
                    `Pedersen
                      {
                        context;
                        step = Option.value ~default:0 t.step;
                        certs;
                        vinput = t.vinput;
                        voutput = t.voutput;
                      }
                  with Exit ->
                    `WaitingForOtherCertificates cert.message.verification)))
      | `POST ->
          let@ dt cont =
            match dt with None -> precondition_failed | Some x -> cont x
          in
          let@ () = handle_generic_error in
          let@ data = body.run Json.of_string in
          let* () = post_trustee w dt ~token data in
          ok
      | _ -> method_not_allowed)
  | _ -> not_found

let dispatch ~token ~ifmatch endpoint method_ body =
  match endpoint with
  | [] ->
      let@ account = with_administrator token in
      let@ { version; group } = body.run !*group_specification_of_yojson in
      let owners = [ account.id ] in
      let* uuid = Storage.T.new_trustees () in
      let@ s = Storage.T.with_transaction uuid in
      let* () =
        Storage.T.set s Trustees_metadata Value
          { version; group; owners; trustees = None }
      in
      let module G = (val Group.make { version; group }) in
      let* () =
        Storage.T.set s (Trustees_draft G.spec) Value
          { step = 1; mode = `Basic { trustees = [] } }
      in
      return_generic
        { mime = "text/plain"; content = String (Uuid.to_string uuid) }
  | uuid :: endpoint ->
      let@ uuid = Option.unwrap bad_request (Option.wrap Uuid.of_string uuid) in
      let@ s = Storage.T.with_transaction uuid in
      let@ metadata, set_metadata =
       fun cont ->
        let@ m, set = Storage.T.update s Trustees_metadata in
        match Lopt.get_value m with
        | None -> not_found
        | Some x -> cont (x, fun x -> set Value x)
      in
      let { version; group; _ } : trustees_metadata = metadata in
      let module G = (val Group.make { version; group }) in
      dispatch_trustees ~token ~ifmatch endpoint method_ body s
        (metadata, set_metadata)
        (module G)
