(lang dune 3.0)
(name randii)
(version 1.0.0) ;; TODO make sure same as in Utils.Version

(generate_opam_files true)

(source (github KaroshiBee/randii))
(license MIT)
(authors "Simon Parry")
(maintainers "simon.parry@karoshibee.com")
(documentation "https://github.com/KaroshiBee/randii")

(package
 (name randii)
 (synopsis "A pure OCaml port of the Random123 counter based random number generator")
 (description "Currently implements the Threefry 2&4 digit 32&64 bit random number generators")
 (depends
  (integers (>= 0.4.0))
  (dune (> 1.5))
  (alcotest :with-test)
  (zarith (and :with-test (>= 1.12)))
  (bisect_ppx (and :with-test (>= 2.6.0)))
  )
  )
