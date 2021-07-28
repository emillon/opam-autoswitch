(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(source
 (github emillon/opam-autoswitch))

(package
 (name opam-autoswitch)
 (depends bos cmdliner ocaml-version rresult))
