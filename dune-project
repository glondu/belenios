(lang dune 2.7)
(name belenios)
(version 1.12)

(generate_opam_files true)
(allow_approximate_merlin)

(license AGPL-3)
(authors "Stéphane Glondu")
(maintainers "stephane.glondu@inria.fr")

(package
 (name belenios-platform-native)
 (synopsis "Native implementation of the Belenios platform")
 (depends
  (cryptokit (>= 1.14))))

(package
 (name belenios-platform-js)
 (synopsis "JavaScript implementation of the Belenios platform")
 (depends
  (js_of_ocaml (>= 3.7.0))
  (js_of_ocaml-ppx (>= 3.7.0))))

(package
 (name belenios-platform)
 (synopsis "Definition of the Belenios platform"))

(package
 (name belenios)
 (synopsis "Belenios library")
 (depends
  (yojson (>= 1.7.0))
  (uuidm (>= 0.9.7))
  (atdgen (>= 2.2.1))
  (belenios-platform (= :version))))

(package
 (name belenios-tool)
 (synopsis "Belenios command-line tool")
 (depends
  (cmdliner (>= 1.0.4))
  (belenios-platform-native (= :version))
  (belenios (= :version))))

(package
 (name belenios-server)
 (synopsis "Belenios server")
 (depends
  (belenios-platform-native (= :version))
  (belenios (= :version))
  (lwt (>= 5.3.0))
  (calendar (>= 2.04))
  (csv (>= 2.4))
  (eliom (>= 6.12.1))
  (lwt_ppx (>= 2.0.1))))
