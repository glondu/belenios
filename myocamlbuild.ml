open Ocamlbuild_plugin

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
  Cmd (S [A"js_of_ocaml"; P (env "%.byte")])

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
    Echo ([platform_dir / kind / "Platform"; "\n"], mllib)
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

let version_rule () =
  let file = "BUILD" in
  let deps = [file; "src/lib/belenios_version.mli"] in
  let prod = "src/lib/belenios_version.ml" in
  let builder _ _ =
    let version, build =
      let ic = open_in file in
      let version = input_line ic in
      let build = input_line ic in
      close_in ic;
      version, build
    in
    let lines = Printf.([
      sprintf "let version = \"%s\"" version;
      sprintf "let build = \"%s\"" build;
    ]) in
    Echo (lines, prod)
  in
  rule "BUILD -> belenios_version.ml" ~deps ~prod builder

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

    rule "%.atd -> %_t.ml & %_t.mli" ~deps:["%.atd"] ~prods:["%_t.ml"; "%_t.mli"]
      (atdgen_action [A"-t"]);
    rule "%.atd -> %_j.ml & %_j.mli" ~deps:["%.atd"] ~prods:["%_j.ml"; "%_j.mli"]
      (atdgen_action [A"-j"; A"-j-std"]);

    rule "%.byte -> %.js" ~deps:["%.byte"] ~prods:["%.js"] js_of_ocaml;

    rule "%.md -> %.html" ~deps:["%.md"] ~prods:["%.html"]
      (fun env build ->
        Cmd (S [A"markdown"; P (env "%.md"); Sh">"; P (env "%.html")])
      );

    build_rule ();
    version_rule ();
    platform_rules "native";
    platform_rules "js";

    copy_rule "jsbn.js" "ext/booth/js/jscrypto/jsbn.js" "src/static/jsbn.js";
    copy_rule "jsbn2.js" "ext/booth/js/jscrypto/jsbn2.js" "src/static/jsbn2.js";
    copy_rule "sjcl.js" "ext/booth/js/jscrypto/sjcl.js" "src/static/sjcl.js";
    copy_rule "random.js" "src/platform/js/random.js" "src/static/random.js";

    copy_rule "belenios-tool" ("src/tool/tool_cmdline" ^ exe_suffix) "belenios-tool";
    copy_rule "belenios-tool.js" "src/tool/tool_js.js" "src/static/tool_js.js";
    copy_rule "belenios-tool.html" "src/tool/belenios-tool.html" "src/static/belenios-tool.html";

    copy_rule "encrypting.gif" "ext/booth/encrypting.gif" "src/static/encrypting.gif";
    copy_rule "booth.js" "src/booth/booth.js" "src/static/booth.js";
    copy_rule "vote.html" "src/booth/vote.html" "src/static/vote.html";

    copy_rule "tool_js_tkeygen.js" "src/tool/tool_js_tkeygen.js" "src/static/tool_js_tkeygen.js";
    copy_rule "tool_js_credgen.js" "src/tool/tool_js_credgen.js" "src/static/tool_js_credgen.js";
    copy_rule "tool_js_questions.js" "src/tool/tool_js_questions.js" "src/static/tool_js_questions.js";
    copy_rule "tool_js_pd.js" "src/tool/tool_js_pd.js" "src/static/tool_js_pd.js";

    List.iter
      copy_static
      [
        "ext/css/reset.css";
        "ext/css/styled-elements.css";
        "ext/css/style.css";
        "ext/css/superfish.css";
      ]

  | _ -> ()
