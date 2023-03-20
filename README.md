# randii

OCaml port of [Random123](http://www.thesalmons.org/john/random123/releases/latest/docs/index.html) - a counter based random number generator.

This library is a pure OCaml port of the Random123 Threefry algorithms as described in
[Parallel Random Numbers: As Easy as 1, 2, 3, Salmon, Moraes, Dror & Shaw, SC11, Seattle, Washington, USA, 2011, ACM](http://dl.acm.org/citation.cfm?doid=2063405)
and originally implemented [here](https://github.com/DEShawResearch/random123).

In terms of number representations this library depends only on [ocaml-integers](https://github.com/ocamllabs/ocaml-integers)
with an additional dependency on [zarith](https://github.com/ocaml/Zarith) if one wants to run the test suite.

## Install

Two options, 

1) via ```opam``` as usual:

```sh
$ opam install randii
```

2) checkout source code and build locally with dune:

```sh
$ git clone https://github.com/KaroshiBee/randii.git
$ cd randii
$ opam install . --deps-only --with-test
$ dune build -w
etc
```

## Testing

For source code checkouts there are unit tests:

```sh
$ cd randii
$ dune runtest -f
```

The test suite uses Known Answer Tests from the test suite of the
[original implementation](https://github.com/DEShawResearch/random123/tree/main/tests) - specifically the `Threefry` data in `kat_vectors` and `old_kat_vectors`.

### CLI

There is a simple CLI tool for basic visual testing/playing: 

```sh
# generate 12 uniforms between [0, 100] with Threefry 4 digit 32 bit RNG, with the default key and starting at ctr={1,2,3,4}
$ randii uniform --upper=100 --rng=threefry4x32 -n 12 -c 1 -c 2 -c 3 -c 4

19
81
16
83
10
34
64
4
17
34
38
85

# generate the first 4 only (4 digit RNGs generate 4 numbers for each (key, ctr) pair)
$ randii uniform --upper=100 --rng=threefry4x32 -n 4 -c 1 -c 2 -c 3 -c 4

19
81
16
83

# jump and generate the last four only
$ randii uniform --upper=100 --rng=threefry4x32 -n 4 -c 3 -c 2 -c 3 -c 4

17
34
38
85

```

Please see:
``` sh
$ randii --help
```

for more information.

### SmallCrush (local checkout only)

There is also an executable for running the Threefry implementations through the SmallCrush battery of tests via OCaml [testu01](https://github.com/LesBoloss-es/ocaml-testu01/):

``` sh
$ dune exec -- u01/smallcrush.exe
```

WARNING it takes a while to run.

## TODOs

- basic arithmetic for ctr_t type,
- make dependence on testu01 a with-test dep
