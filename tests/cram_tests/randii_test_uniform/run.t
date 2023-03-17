testing the uniform generation subcommand defaults
  $ randii uniform
  
  1
  8

testing the uniform generation subcommand with args (2 digit)
  $ for name in 2x32 2x64; do echo "\n\nthreefry_$name" && randii uniform -k 1 -k 2 -c 10 -c 20 -n 10 -r threefry$name --upper 100; done
  
  
  threefry_2x32
  
  44
  87
  11
  80
  24
  75
  77
  83
  93
  52
  
  
  threefry_2x64
  
  58
  71
  67
  20
  83
  31
  33
  70
  25
  49


testing the uniform generation subcommand errors (2 digit)
  $ for name in 2x32 2x64; do echo "\n\nthreefry_$name" && randii uniform -k 1 -k 2 -c 10 -c 20 -c 30 -n 10 -r threefry$name --upper 100; done
  
  
  threefry_2x32
  randii: General error: Need 2 digit ctr/key
  Usage: randii uniform [OPTION]…
  Try 'randii uniform --help' or 'randii --help' for more information.
  
  
  threefry_2x64
  randii: General error: Need 2 digit ctr/key
  Usage: randii uniform [OPTION]…
  Try 'randii uniform --help' or 'randii --help' for more information.
  [124]


testing the uniform generation subcommand with args (4 digit)
  $ for name in 4x32 4x64; do echo "\n\nthreefry_$name" && randii uniform -k 1 -k 2 -k 3 -k 4 -c 10 -c 20 -c 30 -c 40 -n 10 -r threefry$name --upper 100; done
  
  
  threefry_4x32
  
  20
  60
  97
  83
  67
  34
  79
  99
  94
  92
  
  
  threefry_4x64
  
  88
  6
  96
  27
  10
  43
  7
  67
  51
  37


testing the uniform generation subcommand errors (4 digit)
  $ for name in 4x32 4x64; do echo "\n\nthreefry_$name" && randii uniform -k 1 -k 2 -k 3 -k 4 -c 10 -c 20 -n 10 -r threefry$name --upper 100; done
  
  
  threefry_4x32
  randii: General error: Need 4 digit ctr/key
  Usage: randii uniform [OPTION]…
  Try 'randii uniform --help' or 'randii --help' for more information.
  
  
  threefry_4x64
  randii: General error: Need 4 digit ctr/key
  Usage: randii uniform [OPTION]…
  Try 'randii uniform --help' or 'randii --help' for more information.
  [124]
