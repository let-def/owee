#include <dlfcn.h>
#include <caml/memory.h>

CAMLprim value
ml_owee_get_symbol (value name)
{
  CAMLparam1 (name);
  CAMLlocal1 (ret);
  ret = (value) dlsym (NULL, String_val (name)) | 1;
  CAMLreturn (ret);
}
