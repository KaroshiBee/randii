//#include "cstub.h"
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

uint rng_2x32 (void) {
  static const value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("rng_2x32");
  return 1;
//  value result = caml_callback_exn(*closure, caml_copy_string(name));
//  return (uint32) result;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
