open Belenios_platform

let () =
  match Libsodium_stubs.make () with
  | None -> failwith "no libsodium stubs found"
  | Some b ->
      let module B = (val b) in
      let module X = Ed25519_libsodium.Make (B) in
      ()
