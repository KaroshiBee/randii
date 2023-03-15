#include <stdio.h>
#include <randii-u01.h>
#include "unif01.h"
#include "bbattery.h"

int main(int argc, char** argv) {
  initialize_randii(argv);

  unif01_Gen *gen;
  gen = unif01_CreateExternGenBitsL ("Threefry_2x32", rng_2x32);
  bbattery_SmallCrush (gen);
  unif01_DeleteExternGenBitsL (gen);

  return 0;
}

/* int main(int argc, char** argv) { */
/*   initialize_randii(argv); */

/*   unsigned long i = rng_2x32(); */
/*   printf("%lu\n", i); */
/*   printf("\n"); */

/*   i = rng_2x32(); */
/*   printf("%lu\n", i); */
/*   printf("\n"); */

/*   i = rng_2x32(); */
/*   printf("%lu\n", i); */
/*   printf("\n"); */

/*   i = rng_2x32(); */
/*   printf("%lu\n", i); */
/*   printf("\n"); */

/*   i = rng_2x32(); */
/*   printf("%lu\n", i); */
/*   printf("\n"); */
/*   return 0; */
/* } */
