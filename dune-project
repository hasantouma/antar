(lang dune 2.9)
;          ^^^
; When changing the version, don't forget to regenerate *.opam files
; by running [dune build].

(name antar)

(generate_opam_files true)

(maintainers "Hasan Touma")
(authors "Hasan Touma")
(source (github hasantouma/antar))

(package
  (name antar)
  (synopsis "Antar is a subset programming language of Racket")
  (depends
    ppx_blob
    ppx_deriving
    core
    dune
    (menhir
      (>= 2.1))
    (ocaml
      (>= 4.02.3))
    ocamlgraph
    ounit2))

(using menhir 2.1)
