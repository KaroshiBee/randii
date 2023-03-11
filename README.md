# randii

OCaml port of [Random123](http://www.thesalmons.org/john/random123/releases/latest/docs/index.html) - a counter based random number generator.

This library is a pure OCaml port of the Random123 Threefry algorithms as described in
[Parallel Random Numbers: As Easy as 1, 2, 3, Salmon, Moraes, Dror & Shaw, SC11, Seattle, Washington, USA, 2011, ACM](http://dl.acm.org/citation.cfm?doid=2063405)
and originally implemented [here](https://github.com/DEShawResearch/random123).

In terms of number representations this library depends only on [ocaml-integers](https://github.com/ocamllabs/ocaml-integers)
with an additional dependency on [zarith](https://github.com/ocaml/Zarith) if one wants to run the test suite.

There is a simple CLI tool for basic testing, please see 

``` sh
$ randii --help
```

for more information.

The test suite uses Known Answer Tests from the test suite of the
[original implementation](https://github.com/DEShawResearch/random123/tree/main/tests) - specifically the `Threefry` data in `kat_vectors` and `old_kat_vectors`.

## TODOs

- KAT tests are good and give confidence that the ported code does the same calculations as the original implementation
  but it would be good to subject this implementation to the same BIGCRUSH, CRUSH and SMALLCRUSH [statistical tests](http://simul.iro.umontreal.ca/testu01/tu01.html)
  as the original,
- basic arithmetic for ctr_t type,
