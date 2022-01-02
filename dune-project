(lang dune 2.9.1)
;          ^^^^^
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
    ppx_blob ; To store runtime.o at compile time and load it again at runtime
    ppx_deriving ; Testing derives pp for lists
    core ; To determine OS name and read results from terminal in src/xlang/*
    dune ; Build system
    (ocaml
      (>= 4.02.3))
    ocamlgraph ; For bin/viz.ml and src/rlang/viz.ml
    ounit2)) ; Testing module

