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
  (core :with-test)
  dune
  (menhir (>= 2.1))
  (ocaml (>= 4.02.3))
  (ocamlgraph :with-test)
  (ounit2 :with-test)))
(using menhir 2.1)
