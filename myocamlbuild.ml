open Ocamlbuild_plugin

let javascript_libs = [
    "ext/sjcl/sjcl.js";
    "ext/jsbn/BigIntCompatFull.js";
    "src/platform/js/random.js";
  ]

let javascript_to_link =
  S (List.map (fun x -> P x) javascript_libs)

let debug =
  try Sys.getenv "BELENIOS_DEBUG" <> ""
  with Not_found -> false

let try_exec cmd =
  Sys.command (cmd ^ " >/dev/null 2>&1") = 0

let has_ocamlopt = try_exec "which ocamlopt"

let native_compilation =
  try Sys.getenv "OCAMLBEST" = "native"
  with Not_found -> has_ocamlopt

let exe_suffix = if native_compilation then ".native" else ".byte"

let atdgen_action opts env build =
  let x = env "%.atd" in
  let d = Pathname.dirname x and f = Pathname.basename x in
  Cmd (S [A"cd"; P d; Sh"&&"; A"atdgen"; S opts; P f])

let js_of_ocaml env build =
  Cmd (S [A"js_of_ocaml"; javascript_to_link; P (env "%.byte")])

let ( / ) = Filename.concat

let platform_rules kind =
  let lib = "src" / "lib" in
  let platform_dir = "src" / "platform" in
  let platform_mod = platform_dir / kind / "platform" in
  let platform_lib = platform_dir / "platform-" ^ kind in
  let ml = platform_mod ^ ".ml" in
  let mli = platform_mod ^ ".mli" in
  let mllib = platform_lib ^ ".mllib" in
  rule mllib ~deps:[ml] ~prods:[mllib] (fun _ _ ->
    (* technically, there is no dependency, but we need the directory to
       exist for the following *)
    Echo ([platform_dir / kind / "Belenios_version"; "\n";
           platform_dir / kind / "Platform"; "\n"],
          mllib)
  );
  dep ["file:" ^ ml] [mli];
  copy_rule mli (lib / "platform.mli") mli;
  ocaml_lib platform_lib

let build_rule () =
  let genversion = "genversion.sh" in
  let deps = ["VERSION"; genversion] in
  let prod = "BUILD" in
  let builder _ _ =
    Cmd (S [A "sh"; P genversion; Sh ">"; P prod])
  in
  rule "BUILD" ~deps ~prod builder

let read_build () =
  match string_list_of_file "BUILD" with
  | version :: build :: _ -> version, build
  | _ -> failwith "File BUILD is too short!"

let version_rules kind =
  let deps = ["BUILD"; "src/platform/" ^ kind ^ "/belenios_version.mli"] in
  let prod = "src/platform/" ^ kind ^ "/belenios_version.ml" in
  let builder _ _ =
    let version, build = read_build () in
    let lines = Printf.([
      sprintf "let version = \"%s\"" version;
      sprintf "let build = \"%s\"" build;
      sprintf "let debug = %b" debug;
    ]) in
    Echo (lines, prod)
  in
  copy_rule
    (kind / "belenios_tool.mli")
    "src/lib/belenios_version.mli"
    ("src/platform/" ^ kind ^ "/belenios_version.mli");
  rule ("BUILD -> " ^ kind ^ "/belenios_version.ml") ~deps ~prod builder

let meta_rule () =
  let meta_in = "META.in" in
  let deps = [meta_in; "BUILD"] in
  let prod = "lib/belenios/META" in
  let builder _ _ =
    let version, _ = read_build () in
    Cmd (S [A "sed"; A (Printf.sprintf "s/@VERSION@/%s/" version); P meta_in; Sh ">"; P prod])
  in
  rule "META" ~deps ~prod builder

let sjcl_rule () =
  let deps = ["ext/sjcl/core/sjcl.otarget"] in
  let prod = "ext/sjcl/sjcl.js" in
  let builder _ _ =
    let files =
      string_list_of_file "ext/sjcl/core/sjcl.itarget"
      |> List.map (fun x -> P ("ext/sjcl/core" / x))
    in
    Seq [
        Cmd (S [A "cat"; S files; Sh ">"; P prod]);
        Cmd (S [A "echo"; A "belenios.sjcl = sjcl;"; Sh ">>"; P prod]);
      ]
  in
  rule "sjcl.js" ~deps ~prod builder

let bigint_rule () =
  let deps = ["ext/jsbn/BigIntCompat.otarget"] in
  let prod = "ext/jsbn/BigIntCompatFull.js" in
  let builder _ _ =
    let files =
      string_list_of_file "ext/jsbn/BigIntCompat.itarget"
      |> List.map (fun x -> P ("ext/jsbn" / x))
    in
    Seq [
        Cmd (S [A "cat"; S files; Sh ">"; P prod]);
      ]
  in
  rule "BigIntCompatFull.js" ~deps ~prod builder

let wrap_tool name =
  let full_name = "tool_js_" ^ name ^ ".js" in
  let dep = "src/tool" / full_name in
  let deps = [dep] in
  let prod = "src/static" / full_name in
  let builder _ _ =
    Seq [
        (* FIXME: the following is fragile and should be done by js_of_ocaml itself *)
        Cmd (S [A "echo"; A "\"use strict\";(function(g){var belenios={};"; Sh ">"; P prod]);
        Cmd (S [A "sed"; A "s/(function(){return this}())/(g)/g"; P dep; Sh ">>"; P prod]);
        Cmd (S [A "echo"; A "}(this));"; Sh ">>"; P prod]);
      ]
  in
  rule prod ~deps ~prod builder

let copy_static f =
  let base = Filename.basename f in
  copy_rule base f ("src/static" / base)

let () = dispatch & function

  | Before_options ->

    Options.use_ocamlfind := true;
    Options.make_links := false;

  | After_rules ->

    Pathname.define_context "src/web" ["src/lib"];
    Pathname.define_context "src/tool" ["src/lib"];
    Pathname.define_context "src/booth" ["src/lib"];
    Pathname.define_context "demo" ["src/lib"];
    Pathname.define_context "stuff" ["src/lib"];
    Pathname.define_context "." ["src/lib"];

    (* the following avoids an ocamlfind warning, it should be built-in *)
    flag ["doc"; "thread"] (A"-thread");

    (* there seems to be no built-in tag for this... *)
    flag ["compile"; "interf"] (A"-no-keep-locs");

    rule "%.atd -> %_t.ml & %_t.mli" ~deps:["%.atd"] ~prods:["%_t.ml"; "%_t.mli"]
      (atdgen_action [A"-t"]);
    rule "%.atd -> %_j.ml & %_j.mli" ~deps:["%.atd"] ~prods:["%_j.ml"; "%_j.mli"]
      (atdgen_action [A"-j"; A"-j-std"]);

    rule "%.byte -> %.js" ~deps:("%.byte" :: javascript_libs) ~prods:["%.js"] js_of_ocaml;

    rule "%.md -> %.html" ~deps:["%.md"] ~prods:["%.html"]
      (fun env build ->
        Cmd (S [A"markdown"; P (env "%.md"); Sh">"; P (env "%.html")])
      );

    build_rule ();
    meta_rule ();

    sjcl_rule ();
    bigint_rule ();

    version_rules "native";
    version_rules "js";
    platform_rules "native";
    platform_rules "js";

    copy_rule "belenios-tool" ("src/tool/tool_cmdline" ^ exe_suffix) "belenios-tool";
    copy_rule "belenios-tool.js" "src/tool/tool_js.js" "src/static/tool_js.js";
    copy_rule "belenios-tool.html" "src/tool/belenios-tool.html" "src/static/belenios-tool.html";

    copy_rule "encrypting.gif" "ext/images/encrypting.gif" "src/static/encrypting.gif";

    List.iter wrap_tool [
        "booth";
        "tkeygen";
        "ttkeygen";
        "credgen";
        "questions";
        "pd";
        "shuffle";
      ];

    copy_rule "server.cma" "src/web/server.cma" "lib/belenios/server.cma";
    copy_rule "server.cmxs" "src/web/server.cmxs" "lib/belenios/server.cmxs";

    List.iter
      copy_static
      [
        "ext/css/reset.css";
        "ext/css/styled-elements.css";
        "ext/css/style.css";
        "ext/css/superfish.css";
      ]

  | _ -> ()
