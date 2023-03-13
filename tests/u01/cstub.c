//#include "cstub.h"
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/alloc.h>

unsigned int rng_2x32 (void) {
  static const value* closure = NULL;
  if (closure == NULL)
    closure = caml_named_value("rng_2x32");

  char* k1 = "0";
  char* k2 = "0";
  char* c1 = "0";
  char* c2 = "0";
  int index = 0;
  value result = caml_callback_exn(*closure,
                                   (caml_copy_string(k1),
                                    caml_copy_string(k2),
                                    caml_copy_string(c1),
                                    caml_copy_string(c2),
                                    caml_copy_int64(index))
                                   );
  return (unsigned int)result;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
