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
    // printf("%c, ", *x);
    if (!isdigit(*x))
      return 0L;
  }
  // printf("\n");
  return (strtoul(st, NULL, 10));
}

void pp(const char *st) {
  // printf("pp: ");
  for (const char* x = st ; *x ; x++) {
    // printf("%c, ", *x);
  }
  // printf("\n");
}

unsigned long rng_2x32 (void) {
  // printf("rng 2x32: start\n");

  // globals - closures in OCaml
  static const value* o_rng = NULL;
  if (o_rng == NULL) {
    // printf("rng 2x32: getting 'rng' closure\n");
    o_rng = caml_named_value("randii_rng_2x32");
  }
  if (o_rng == NULL) {
    // printf("rng 2x32: cannot get ocaml 'rng' closure\n");
    return 0L;
  }

  static const value* o_next = NULL;
  if (o_next == NULL) {
    // printf("rng 2x32: getting 'next' closure\n");
    o_next = caml_named_value("randii_next_2x32");
  }
  if (o_next == NULL) {
    // printf("rng 2x32: cannot get ocaml 'next' closure\n");
    return 0L;
  }

  // globals - data
  static int ix = 0;
  static const char* keys[] = {"0","0", NULL};
  static const char* ctrs[] = {"0","0", NULL};

  // calcs
  pp(ctrs[0]);
  pp(ctrs[1]);
  value o_keys = make_str_array(keys);
  value o_ctrs = make_str_array(ctrs);
  value o_ix = Val_int(ix);
  value o_result = caml_callback3(*o_rng, o_keys, o_ctrs, o_ix);
  // NOTE first one should be 1797259609
  const unsigned long i = convert(String_val(o_result));
  // printf("i, index: %lu, %d\n", i, ix);

  if (1 == ix) {
    // printf("incr ctr\n");
    o_ctrs = caml_callback(*o_next, o_ctrs);
    ctrs[0] = String_val(Field(o_ctrs, 0));
    ctrs[1] = String_val(Field(o_ctrs, 1));
  }
  // printf("switch index\n");
  ix++;
  ix = ix % 2;
  return i;
}

void initialize_randii (char** argv) {
  caml_startup(argv);
}
