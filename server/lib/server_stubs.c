#include <util.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

char* argv[] = { "ls" };
char* envp[] = { "TERM=xterm" };

value fork_in_pty(
    value v_working_dir,
    value v_prog,
    value v_args,
    value v_env,
    value v_dim
    ) {
  int master_fd = -1;
  pid_t child_pid = -1;

  char* name = malloc(sizeof(char)*1024);

  struct winsize* winp = malloc(sizeof(struct winsize));
  winp->ws_col = Int_val(Field(v_env, 1));
  winp->ws_row = Int_val(Field(v_env, 2));

  if ((child_pid = forkpty(&master_fd, name, NULL, winp)) == 0) {

    // CR datkin: Unsure about the following signal handler resets...
    signal(SIGCHLD, SIG_DFL);
    signal(SIGHUP, SIG_DFL);
    signal(SIGINT, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGTERM, SIG_DFL);
    signal(SIGALRM, SIG_DFL);

    char* working_dir = String_val(v_working_dir);
    if (chdir(working_dir) < 0) {
      caml_failwith("dir doesn't exist");
    }

    int n_args = Wosize_val(v_args);
    char* args[n_args+1];
    args[n_args] = NULL;

    for (int i = 0; i < n_args; i++) {
      args[i] = String_val(Field(v_args, i));
    }

    int n_env = Wosize_val(v_env);
    char* env[n_env+1];
    env[n_env] = NULL;

    for (int i = 0; i < n_env; i++) {
      env[i] = String_val(Field(v_env, i));
    }

    char* prog = String_val(v_prog);

    execve(prog, args, env);
  }

  //ioctl(master_fd, TIOCSWINSZ, &winp);

  value v_name = caml_copy_string(name);
  value v_res = caml_alloc_small(4, 0);
  Field(v_res, 0) = Val_int(master_fd);
  Field(v_res, 1) = Val_int(child_pid);
  Field(v_res, 2) = v_name;

  return v_res;
}
