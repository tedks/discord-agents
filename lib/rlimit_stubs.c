#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/resource.h>

static int64_t rlim_to_int64(rlim_t value) {
  if (value == RLIM_INFINITY) {
    return INT64_MAX;
  }
  if (sizeof(rlim_t) > sizeof(int64_t) && value > (rlim_t)INT64_MAX) {
    return INT64_MAX;
  }
  return (int64_t)value;
}

static rlim_t int64_to_rlim(int64_t value) {
  if (value < 0) {
    caml_invalid_argument("nofile limit must be non-negative");
  }
  if ((uint64_t)value > (uint64_t)RLIM_INFINITY) {
    return RLIM_INFINITY;
  }
  return (rlim_t)value;
}

CAMLprim value discord_agents_get_nofile_limit(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(result, soft, hard);
  struct rlimit limit;

  if (getrlimit(RLIMIT_NOFILE, &limit) != 0) {
    caml_failwith(strerror(errno));
  }

  soft = caml_copy_int64(rlim_to_int64(limit.rlim_cur));
  hard = caml_copy_int64(rlim_to_int64(limit.rlim_max));
  result = caml_alloc_tuple(2);
  Store_field(result, 0, soft);
  Store_field(result, 1, hard);
  CAMLreturn(result);
}

CAMLprim value discord_agents_set_nofile_soft_limit(value soft_v) {
  CAMLparam1(soft_v);
  struct rlimit limit;

  if (getrlimit(RLIMIT_NOFILE, &limit) != 0) {
    caml_failwith(strerror(errno));
  }

  limit.rlim_cur = int64_to_rlim(Int64_val(soft_v));
  if (setrlimit(RLIMIT_NOFILE, &limit) != 0) {
    caml_failwith(strerror(errno));
  }

  CAMLreturn(Val_unit);
}
