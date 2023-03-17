testing the uniform01 generation subcommand defaults
  $ randii uniform01
  
  0.156378186308
  0.372508365661

testing the uniform01 generation subcommand with args (2 digit)
  $ for name in 2x32 2x64; do echo "threefry_$name" && randii uniform01 -k 1 -k 2 -c 10 -c 20 -n 10 -r threefry$name; done
  threefry_2x32
  
  0.0549717284739
  0.838563348167
  0.931870128959
  0.617888104171
  0.193101960234
  0.541362379678
  0.045745014213
  0.791789597832
  0.332666493021
  0.192312333733
  threefry_2x64
  
  0.965788019821
  0.775123058818
  0.223426050507
  0.284003831446
  0.274588554166
  0.106841350906
  0.0489657195285
  0.270103232935
  0.611602862366
  0.697210346349

testing the uniform01 generation subcommand errors (2 digit)
  $ for name in 2x32 2x64; do echo "threefry_$name" && randii uniform01 -k 1 -k 2 -c 10 -c 20 -c 30 -n 10 -r threefry$name; done
  threefry_2x32
  randii: General error: Need 2 digit ctr/key
  Usage: randii uniform01 [OPTION]…
  Try 'randii uniform01 --help' or 'randii --help' for more information.
  threefry_2x64
  randii: General error: Need 2 digit ctr/key
  Usage: randii uniform01 [OPTION]…
  Try 'randii uniform01 --help' or 'randii --help' for more information.
  [124]

testing the uniform01 generation subcommand with args (4 digit)
  $ for name in 4x32 4x64; do echo "threefry_$name" && randii uniform01 -k 1 -k 2 -k 3 -k 4 -c 10 -c 20 -c 30 -c 40 -n 10 -r threefry$name; done
  threefry_4x32
  
  0.246938243508
  0.659323853441
  0.944508946501
  0.548915906809
  0.0992874708027
  0.936817220412
  0.841072714888
  0.145545294508
  0.572219017893
  0.809060395695
  threefry_4x64
  
  0.335126876831
  0.366305815056
  0.594112969935
  0.620414878242
  0.501386901364
  0.562692156993
  0.859386133961
  0.018513510935
  0.121436684392
  0.676166095771

testing the uniform01 generation subcommand errors (4 digit)
  $ for name in 4x32 4x64; do echo "threefry_$name" && randii uniform01 -k 1 -k 2 -c 10 -c 20 -c 30 -n 10 -r threefry$name; done
  threefry_4x32
  randii: General error: Need 4 digit ctr/key
  Usage: randii uniform01 [OPTION]…
  Try 'randii uniform01 --help' or 'randii --help' for more information.
  threefry_4x64
  randii: General error: Need 4 digit ctr/key
  Usage: randii uniform01 [OPTION]…
  Try 'randii uniform01 --help' or 'randii --help' for more information.
  [124]
