(lang dune 3.17)

(name lexie)

(generate_opam_files true)

(source
 (github qexat/lexie))

(authors "lexa <contact@qexat.com>")

(maintainers "lexa <contact@qexat.com>")

(license LICENSE)

(package
 (name lexie)
 (synopsis "a ML-style, CIC-based programming language")
 (description "i'll put something here later")
 (depends
  ocaml
  (cmdliner
   (>= 1.3.0))))
