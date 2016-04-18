#include <util.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

char* argv[] = { "ls" };
char* envp[] = { "TERM=xterm" };

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
