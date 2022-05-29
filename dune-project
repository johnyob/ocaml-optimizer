(lang dune 2.9)
(name optimizer)

(generate_opam_files true)

(maintainers "ajo41@cam.ac.uk")

(authors "Alistair O'Brien")


(package
 (name optimizer)
 (depends
  (ocaml
   (= 4.12.0))
  (dune
   (>= 2.9))))
(using menhir 2.1)
