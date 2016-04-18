#include <util.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

char* argv[] = { "ls" };

int main(int argc, char** argv) {
  execve("/bin/ls", argv, NULL);
  /*
  int* stat_loc;
  int* master_fd;
  //char* arg0 = "ls";
  pid_t pid = forkpty(master_fd, NULL, NULL, NULL);
  if (pid == 0) {
    execve("/bin/ls", argv, NULL);
  } else {
    waitpid(pid, stat_loc, 0);
    return 0;
  }
  */
}
