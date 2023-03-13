//#include "cstub.h"
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <string.h>

value make_str (const char *s) { return caml_copy_string(s); }
value make_str_array (const char **p) { return caml_alloc_array(make_str, p); }


unsigned int rng_2x32 (void) {
  printf("rng 2x32\n");
  static const value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("rng_2x32");

  const char* ss[] = {"11","11","11","11", NULL};
  value keys = make_str_array(ss);
  value index = caml_copy_int64(0);
  value result = caml_callback2_exn(*closure, keys, index);
  return (unsigned int)result;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
