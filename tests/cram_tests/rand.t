testing the rand generation subcommand defaults
  $ randii rand
  
  14315441772321446311
  5507775269476707668

testing the rand generation subcommand with args (2 digit)
  $ for name in 2x32 2x64; do echo "threefry_$name" && randii rand -k 1 -k 2 -c 10 -c 20 -n 10 -r threefry$name; done
  threefry_2x32
  
  59025444
  3047884187
  3452736511
  3148071580
  1737194124
  1281083475
  2728767077
  2196601983
  3383652293
  3636753452
  threefry_2x64
  
  16120929490155042958
  7108249018347265471
  8303535089779514567
  13774955366652386920
  2888412248983919583
  16053295177860021531
  13156095983293186333
  6755528685501636370
  10311854920489533525
  4324660322295027749

testing the rand generation subcommand errors (2 digit)
  $ for name in 2x32 2x64; do echo "threefry_$name" && randii rand -k 1 -k 2 -c 10 -c 20 -c 30 -n 10 -r threefry$name; done
  threefry_2x32
  randii: General error: Need 2 digit ctr/key
  Usage: randii rand [OPTION]…
  Try 'randii rand --help' or 'randii --help' for more information.
  threefry_2x64
  randii: General error: Need 2 digit ctr/key
  Usage: randii rand [OPTION]…
  Try 'randii rand --help' or 'randii --help' for more information.
  [124]

testing the rand generation subcommand with args (4 digit)
  $ for name in 4x32 4x64; do echo "threefry_$name" && randii rand -k 1 -k 2 -k 3 -k 4 -c 10 -c 20 -c 30 -c 40 -n 10 -r threefry$name; done
  threefry_4x32
  
  265147920
  4205030860
  707943597
  2087900583
  589393967
  1180350934
  3153383479
  3050578599
  1230019894
  614415492
  threefry_4x64
  
  7977693318990706688
  15374634856540638706
  17222306573226209896
  17275153867351580827
  4423982397508729110
  12413904189250545143
  1541179923027209907
  4602856014510773067
  9671179290204806751
  14702484486999133737

testing the rand generation subcommand errors (4 digit)
  $ for name in 4x32 4x64; do echo "threefry_$name" && randii rand -k 1 -k 2 -c 10 -c 20 -c 30 -n 10 -r threefry$name; done
  threefry_4x32
  randii: General error: Need 4 digit ctr/key
  Usage: randii rand [OPTION]…
  Try 'randii rand --help' or 'randii --help' for more information.
  threefry_4x64
  randii: General error: Need 4 digit ctr/key
  Usage: randii rand [OPTION]…
  Try 'randii rand --help' or 'randii --help' for more information.
  [124]
