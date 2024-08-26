#include <stdlib.h>
#include <stdio.h>

#define CAML_INTERNALS

#include <caml/version.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/address_class.h>
#if OCAML_VERSION >= 41200
#include <caml/codefrag.h>
#endif

/* Use dladdr. Should work at least with Linux, FreeBSD and OS X. */
#define _GNU_SOURCE
/* Because the previous one is sometime ignored, GNU LOLism. */
#define __USE_GNU
#include <dlfcn.h>

/* Assumptions on caml startup code:
 *
 * caml_startup__code_begin
 *  caml_curry_*
 *  caml_tuplify_*
 *  caml_send_*
 *  caml_apply_*
 * caml_startup__code_end
 *
 * When taking code pointer from a closure, check if code pointer lies between
 * caml_startup__code_begin and caml_startup__code_end.
 *
 * If not, return it directly.
 * Otherwise, scan the rest of the fields for a value between
 * caml_code_area_start and caml_code_area_end, use it as the valid code pointer.
 */

void **caml_startup__code_begin_p = NULL;
void **caml_startup__code_end_p = NULL;

void load_caml_startup__code(void)
{
  void *handle;
  char *error;

  handle = dlopen("", RTLD_LAZY);
  if (handle == NULL) {
    fprintf(stderr, "dlopen failed\n");
    return;
  }
  dlerror();
  caml_startup__code_begin_p = dlsym(handle, "caml_startup__code_begin");
  error = dlerror();
  if (error != NULL) {
    return;
  }
  caml_startup__code_end_p = dlsym(handle, "caml_startup__code_end");
  error = dlerror();
  if (error != NULL) {
    return;
  }
  dlclose(handle);
  return;
}

static void *closure_code_pointer(value closure)
{
  unsigned i;
  void *cp = (void*)Field(closure, 0);

  if (caml_startup__code_begin_p == NULL || caml_startup__code_end_p == NULL) {
    return cp;
  }

  /* Normal code pointer */
  if (cp < (void*)caml_startup__code_begin_p ||
      cp > (void*)caml_startup__code_end_p)
    return cp;

  for (i = 1; i < Wosize_val(closure); ++i)
  {
    void *cp2 = (void*)Field(closure, i);
#if OCAML_VERSION >= 41200
    struct code_fragment *cf = caml_find_code_fragment_by_pc(cp2);
    if (cf != NULL)
      return cp2;
#else
    if (cp2 >= (void*)caml_code_area_start && cp2 <= (void*)caml_code_area_end)
      return cp2;
#endif
  }

  return cp;
}

CAMLprim value ml_owee_init(value unit)
{
  load_caml_startup__code();
  return unit;
}

CAMLprim value ml_owee_code_pointer(value closure)
{
  void *result = closure_code_pointer(closure);
  return ((intnat)result | 1);
}

CAMLprim value ml_owee_code_pointer_symbol(value cp)
{
  const char * result = "";
  Dl_info info;

  if ((intnat)cp != 1 &&
      dladdr((void*)cp, &info) != 0 &&
      info.dli_sname != NULL)
  {
    result = info.dli_sname;
  }

  return caml_copy_string(result);
}
