#!/usr/bin/env python3
"""A local stand-in for `ssh` in Host_exec tests.

It accepts the option/`--`/destination/command shape Host_exec uses and
runs the command with `sh -c`, the way sshd hands it to a login shell.
It is a relay, not an exec: the "remote" command runs in its own session
behind pipes this process owns, so killing this process closes the
remote side's stdin and the caller's stdout exactly as a dropped SSH
connection does. A plain `exec sh -c` would leave the remote command
holding the caller's pipes and could not model a lost connection.

Destination "unreachable" behaves like a host with no route.
"""

import os
import subprocess
import sys
import threading


def main():
    args = sys.argv[1:]
    if "--" not in args:
        sys.stderr.write("fake_ssh: expected -- before destination\n")
        return 255
    rest = args[args.index("--") + 1:]
    if not rest:
        sys.stderr.write("fake_ssh: missing destination\n")
        return 255
    destination, command = rest[0], " ".join(rest[1:])
    if destination == "unreachable":
        sys.stderr.write(
            "ssh: connect to host unreachable port 22: No route to host\n")
        return 255

    child = subprocess.Popen(
        ["sh", "-c", command],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        start_new_session=True, bufsize=0)

    def pump(src, dst, on_eof=None):
        try:
            while True:
                chunk = os.read(src, 65536)
                if not chunk:
                    break
                os.write(dst, chunk)
        except OSError:
            pass
        finally:
            if on_eof:
                on_eof()

    def close_child_stdin():
        # Through the file object, so Popen does not close the fd again.
        try:
            child.stdin.close()
        except OSError:
            pass

    stdin_pump = threading.Thread(
        target=pump, args=(0, child.stdin.fileno(), close_child_stdin),
        daemon=True)
    out_pump = threading.Thread(
        target=pump, args=(child.stdout.fileno(), 1))
    err_pump = threading.Thread(
        target=pump, args=(child.stderr.fileno(), 2))
    for t in (stdin_pump, out_pump, err_pump):
        t.start()
    rc = child.wait()
    out_pump.join()
    err_pump.join()
    # ssh reports a remote death by signal as 255.
    return rc if rc >= 0 else 255


if __name__ == "__main__":
    sys.exit(main())
