//#include "cstub.h"
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

uint32 rng_2x32 (void* ) {
  static const value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("rng_2x32");

  value result = caml_callback_exn(*closure, caml_copy_string(name));
  return (char*) result;
}

void initialize_example (char** argv) {
  caml_startup(argv);
}
