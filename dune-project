(lang dune 3.10)

(name rinha_ocaml)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github imaqtkatt/rinha-ocaml))

(package
 (name rinha_ocaml)
 (depends
  (ocaml (= 5.0.0))
  (dune (>= 3.10))
  menhir
  (ppx_deriving (= 5.2.1))))
