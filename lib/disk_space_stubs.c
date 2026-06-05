#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/statvfs.h>

CAMLprim value discord_agents_available_bytes(value path_v)
{
  CAMLparam1(path_v);
  struct statvfs st;
  const char *path = String_val(path_v);

  if (statvfs(path, &st) != 0) {
    caml_failwith(strerror(errno));
  }

  unsigned __int128 bytes =
    ((unsigned __int128) st.f_bavail) * ((unsigned __int128) st.f_frsize);
  if (bytes > (unsigned __int128) INT64_MAX) {
    bytes = (unsigned __int128) INT64_MAX;
  }

  CAMLreturn(caml_copy_int64((int64_t) bytes));
}
