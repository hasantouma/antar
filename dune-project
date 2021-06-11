(lang dune 2.8)
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
  ; The "depends" and "build" field are written in dune.opam.template
  (synopsis "Antar is a subset programming language of Racket")
  (depends
    (ocaml
      (>= 4.02.3))
    dune
    ounit2))

