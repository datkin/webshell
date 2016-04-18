#include <util.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

char* argv[] = { "ls" };
char* envp[] = { "TERM=xterm" };

value fork_in_pty( value v_working_dir, value v_prog, value v_args, value v_env) {
  //CAMLparam1(v_working_dir); //, v_prog, v_args, v_env);

  int master_fd = -1;
  pid_t child_pid = -1;

  if ((child_pid = forkpty(&master_fd, NULL, NULL, NULL)) == 0) {

    char* working_dir = String_val(v_working_dir);
    if (chdir(working_dir) < 0) {
      caml_failwith("dir doesn't exist");
    }

    int n_args = Wosize_val(v_args);
    char* args[n_args+1];
    args[n_args+1] = NULL;

    for (int i = 0; i<n_args; i++) {
      args[i] = String_val(Field(v_args, i));
    }

    //int n_env = Wosize(v_env);
    //char

  }


  value v_res = caml_alloc_small(4, 0);
  Field(v_res, 0) = Val_int(master_fd);
  Field(v_res, 1) = Val_int(child_pid);

  return v_res;
}

int main(int argc, char** argv) {
  int master_fd = -1;
  pid_t pid = forkpty(&master_fd, NULL, NULL, NULL);
  if (pid == 0) {
    execve("/usr/bin/clear", argv, envp);
  } else {
    int* stat_loc = NULL;
    size_t buf_size = 1024;
    char* buf = malloc(buf_size);
    size_t size_read = 0;
    unsigned int total = 0;
    while ((size_read = read(master_fd, buf, buf_size)) > 0) {
      for (int i = 0; i < size_read; i++) {
        if (total % 10 == 0) { printf("\n"); }
        printf(" %02x (%c)", buf[i], buf[i]);
        total++;
      }
    }
    printf("\ndone\n");
    waitpid(pid, stat_loc, 0);
    return 0;
  }
}
