(lang dune 2.8)
(name randii)
(version 0.9.0)

(generate_opam_files true)

(source (github wyn/randii))
(license MIT)
(authors "Simon Parry")
(maintainers "simon.parry@karoshibee.com")

(package
 (name randii)
 (synopsis "An OCaml port of the Random123 counter based random number generator")
 (description "A longer description")
 (depends
  (integers (>= 0.4.0))
  (dune (> 1.5))
  (alcotest :with-test)
  (zarith (and :with-test (>= 1.12)))
  (bisect_ppx (and :with-test (>= 2.6.0)))
  )
  )
