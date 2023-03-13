#include <stdio.h>
#include <randii-u01.h>

int main(int argc, char** argv) {
  initialize_randii(argv);
  const unsigned int i = rng_2x32();
  printf("\n%d\n", i);
  return 0;
}
