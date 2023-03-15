#include <stdio.h>
#include <randii-u01.h>

int main(int argc, char** argv) {
  initialize_randii(argv);

  unsigned long i = rng_2x32();
  printf("%lu\n", i);
  printf("\n");

  i = rng_2x32();
  printf("%lu\n", i);
  printf("\n");

  i = rng_2x32();
  printf("%lu\n", i);
  printf("\n");

  i = rng_2x32();
  printf("%lu\n", i);
  printf("\n");

  i = rng_2x32();
  printf("%lu\n", i);
  printf("\n");
  return 0;
}
