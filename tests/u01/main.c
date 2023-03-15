#include <stdio.h>
#include <randii-u01.h>

int main(int argc, char** argv) {
  initialize_randii(argv);
  const unsigned long i = rng_2x32();
  printf("\n%lu\n", i);
  return 0;
}
