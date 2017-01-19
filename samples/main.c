#include <curses.h>
#include <term.h>
#include <stdio.h>

void dump(char* str) {
  int idx = 0;
  printf("hex   > ");
  while (str[idx]) {
    printf("%02x ", str[idx]);
    idx++;
  }
  idx = 0;
  printf("\nascii > ");
  while (str[idx]) {
    printf("'%c', ", str[idx]);
    idx++;
  }
  printf("\n");
}

int main(int argc, char** argv) {
  /* Here's a program which looks up terminal capabilities and then uses the
   * curses program to evaluate/render string capabilities.
   *
   * It also forms some simple-ish custom capability strings and evaluates them
   * just to test how the evaluator works.
   *
   * See `man curs_terminfo` and `man terminfo`. */
  setterm("xterm");
  char* str;
  str = tigetstr("cud");
  dump(str);
  str = tparm(str, 1);
  dump(str);

  char* test;
  // This pushes the sum of arg1 and arg2 to the stack, and then prints it.
  test = tparm ("%p2%p1%-%d", 8, 2);
  dump(test);

  // Same as above, but with string padding.
  test = tparm ("%p2%p1%-%10d", 8, 2);
  dump(test);

  test = tparm ("%p1", 10);
  dump(test);

  test = tparm ("%p1%d", 10);
  dump(test);

  // supposedly %l pushes to the stack, but this example shows %l printing
  test = tparm ("%p2%l", "a", "abc");
  dump(test);

  str = tigetstr("sgr");
  test = tparm(str, 1, 0, 0, 0, 0, 0, 0, 0, 0);
  dump(test);

  printf("done\n");
  return 0;
}
