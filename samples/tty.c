/* #include <dlfcn.h> */
#include <libproc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/msg.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/user.h>
#include <unistd.h>
#include <signal.h>
#include <util.h>
#include <fcntl.h>

void handle_sigchld(int n) {
  printf("Got SIGCHLD %d\n", n);
}

int main(int argc, char** argv) {
  int fd;
  struct winsize win = { 30, 30, 0, 0 };

  signal(SIGCHLD, handle_sigchld);

  char* tty_name = malloc(sizeof(char)*1024);

  int pid = forkpty(&fd, tty_name, NULL, &win);

  if (pid == 0) {
    signal (SIGCHLD, SIG_DFL);
    signal (SIGPIPE, SIG_DFL);
    sigset_t signals;
    sigemptyset(&signals);
    sigaddset(&signals, SIGPIPE);
    sigprocmask(SIG_UNBLOCK, &signals, NULL);

    chdir("/tmp");

    char** env = malloc(sizeof(char*)*1);
    env[0] = "TERM=xterm";

    char** args = malloc(sizeof(char*)*1);
    args[0] = "nano";

    execve("/usr/bin/nano", args, env);
    _exit(-1);
  } else if (pid < 0) {
    exit(3);
  }

  if (fcntl(fd, F_SETFL, O_NONBLOCK) < 0) {
    exit(4);
  }

  if (ioctl(fd, TIOCSWINSZ, &win) < 0) {
    exit(5);
  }

  char buf[1024];

  printf("Terminal: %s\n", tty_name);

  int printed = 0;

  for (;;) {
    int n = read(fd, buf, 1024);
    /*
    if(n < 0) {
      exit(6);
    } else */ if (n <= 0) {
      usleep(10);
    } else {
      for (int i = 0; i < n; i++) {
        printf("'%c', ", buf[i]);
        printed++;
        if(printed % 20 == 0) {
          printf("\n");
        }
      }
    }
  }
}
