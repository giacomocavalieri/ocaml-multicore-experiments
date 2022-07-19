(lang dune 3.3)

(name multicore)

(generate_opam_files true)

(source
 (github giacomocavalieri/ocaml-multicore-experiments))

(authors "Giacomo Cavalieri")

(package
 (name multicore)
 (synopsis
  "Project to experiment with OCaml 5 zeroth release's support to multicore")
 (depends ocaml dune)
 (tags (multicore)))
