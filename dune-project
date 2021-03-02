(lang dune 2.7)
(name oak)

(generate_opam_files true)

(source (github imeckler/oak))
(license MIT)
(authors "Izaak Meckler")
(maintainers "ihmeckler@gmail.com")

(package
 (name oak)
 (synopsis "An OCaml library for declarative canvas")
 (description "A longer description")
 (depends
  core_kernel))
