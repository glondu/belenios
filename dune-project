(lang dune 3.11)
(using menhir 2.0)
(name belenios)
(version 2.5.1)

(generate_opam_files true)

(license AGPL-3)
(authors "Stéphane Glondu")
(maintainers "stephane.glondu@inria.fr")

(package
 (name belenios-platform-native)
 (synopsis "Native implementation of the Belenios platform")
 (depends
  (cryptokit (>= 1.17))))

(package
 (name belenios-platform-js)
 (synopsis "JavaScript implementation of the Belenios platform")
 (depends
  (js_of_ocaml (>= 4.0.0))
  (js_of_ocaml-ppx (>= 4.0.0))))

(package
 (name belenios-platform)
 (synopsis "Definition of the Belenios platform"))

(package
 (name belenios-lib)
 (synopsis "Belenios library")
 (depends
  (yojson (>= 2.0.2))
  (atdgen (>= 2.10.0))
  (belenios-platform (= :version))))

(package
 (name belenios-tool)
 (synopsis "Belenios command-line tool")
 (depends
  (cmdliner (>= 1.1.0))
  (belenios-platform-native (= :version))
  (belenios-lib (= :version))))

(package
 (name belenios-server)
 (synopsis "Belenios server")
 (depends
  (belenios-platform-native (= :version))
  (belenios-lib (= :version))
  (lwt (>= 5.6.1))
  (calendar (>= 2.04))
  (csv (>= 2.4))
  (eliom (>= 10.0.0))
  (ocamlnet (>= 4.1.9-1))))

(package
 (name belenios)
 (synopsis "Belenios meta-package")
 (allow_empty)
 (depends
  (belenios-tool (= :version))
  (belenios-server (= :version))))
