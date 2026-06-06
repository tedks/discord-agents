#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/statvfs.h>

CAMLprim value discord_agents_available_bytes(value path_v)
{
  CAMLparam1(path_v);
  CAMLlocal1(result);
  struct statvfs st;
  char *path = strdup(String_val(path_v));
  if (path == NULL) {
    caml_failwith("out of memory");
  }

  int rc;
  int saved_errno;
  caml_enter_blocking_section();
  rc = statvfs(path, &st);
  saved_errno = errno;
  caml_leave_blocking_section();
  free(path);

  if (rc != 0) {
    caml_failwith(strerror(saved_errno));
  }

  unsigned __int128 bytes =
    ((unsigned __int128) st.f_bavail) * ((unsigned __int128) st.f_frsize);
  if (bytes > (unsigned __int128) INT64_MAX) {
    bytes = (unsigned __int128) INT64_MAX;
  }

  result = caml_copy_int64((int64_t) bytes);
  CAMLreturn(result);
}
