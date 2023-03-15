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

static int ix = 0;
static char* keys[] = {"0","0", NULL};
static char* ctrs[] = {"0","0", NULL};

unsigned long rng_2x32 (void) {
  printf("rng 2x32: start\n");
  static const value* o_closure = NULL;
  if (o_closure == NULL)
    o_closure = caml_named_value("randii_rng_2x32");
  if (o_closure == NULL) {
    printf("rng 2x32: cannot get ocaml closure\n");
    return 0L;
  }

  value o_keys = make_str_array((const char**)keys);
  value o_ctrs = make_str_array((const char**)ctrs);
  value o_ix = Val_int(ix);

  value o_result = caml_callback3(*o_closure, o_keys, o_ctrs, o_ix);
  /* if (Is_exception_result(result)) { */
  /*   printf("Got exception\n"); */
  /*   return 0; */
  /* } */

  // 2579123966
  const char* s = String_val(o_result);
  const unsigned long i = convert(s);
  printf("i, index: %lu, %d\n", i, ix);
  if (1 == ix) {
    printf("incr ctr\n");
  }
  ix++;
  ix = ix % 2;
  return i;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
