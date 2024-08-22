(lang dune 3.0)
(name owee)
(generate_opam_files true)

(license MIT)
(maintainers "Frédéric Bour <frederic.bour@lakaban.net>")
(authors "Frédéric Bour <frederic.bour@lakaban.net>")
(source (github let-def/owee))

(package
  (name owee)
  (depends
    (ocaml (>= 4.08))
    (cmdliner :with-test))
  (synopsis "OCaml library to work with DWARF format")
  (description "
Owee is an experimental library to work with DWARF format.
It can parse ELF binaries and interpret DWARF debugline programs.

It can also be used to find locations of functions from the current process."))