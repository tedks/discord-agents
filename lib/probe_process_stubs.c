#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>

/* Observe exit without releasing the PID. Cleanup must signal the group
   while its leader's PID is still reserved, then reap with waitpid. */
CAMLprim value discord_agents_probe_exited(value pid_v) {
  siginfo_t info;
  int rc;
  do {
    memset(&info, 0, sizeof(info));
    rc = waitid(P_PID, Int_val(pid_v), &info, WEXITED | WNOHANG | WNOWAIT);
  } while (rc < 0 && errno == EINTR);
  if (rc < 0) caml_uerror("waitid", Nothing);
  return Val_bool(info.si_pid != 0);
}

CAMLprim value discord_agents_probe_children_waitable(value unit) {
  struct sigaction action;
  if (sigaction(SIGCHLD, NULL, &action) < 0) caml_uerror("sigaction", Nothing);
  return Val_bool(action.sa_handler != SIG_IGN && !(action.sa_flags & SA_NOCLDWAIT));
}
