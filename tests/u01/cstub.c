//#include "cstub.h"
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <string.h>
#include <ctype.h>

value make_str (const char *s) {
  return caml_copy_string(s);
}

value make_str_array (const char **p) {
  return caml_alloc_array(make_str, p);
}

unsigned long convert(const char *st) {
  for (const char* x = st ; *x ; x++) {
    printf("%c, ", *x);
    if (!isdigit(*x))
      return 0L;
  }
  printf("\n");
  return (strtoul(st, NULL, 10));
}

unsigned long rng_2x32 (void) {
  printf("rng 2x32: start\n");
  static const value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("randii_rng_2x32");
  if (closure == NULL) {
    printf("rng 2x32: cannot get ocaml closure\n");
    return 0L;
  }

  // first 2 are keys, final two are ctrs
  const char* ss[] = {"0","0","0","0", NULL};
  value keys = make_str_array(ss);
  value index = Val_int(1);
  value result = caml_callback2(*closure, keys, index);
  /* if (Is_exception_result(result)) { */
  /*   printf("Got exception\n"); */
  /*   return 0; */
  /* } */

  // 2579123966
  const char* s = String_val(result);
  const unsigned long i = convert(s);
  printf("i: %lu\n", i);
  return i;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
