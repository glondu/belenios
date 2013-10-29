let usage () =
  Printf.eprintf
    "Usage: %s { trustee-keygen | election | credgen } options...\n"
    Sys.argv.(0);
  exit 1


let () =
  let n = Array.length Sys.argv in
  if n < 2 then usage ()
  else (
    Arg.current := 1;
    match Sys.argv.(1) with
    | "trustee-keygen" -> Tkeygen.main ()
    | "election" -> Tool.main ()
    | "credgen" -> Credgen.main ()
    | _ -> usage ()
  )
