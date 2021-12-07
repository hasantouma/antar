#include <stdio.h>

int entry();

int read_int() {
  int x;
  scanf("%d", &x);

  return x;
}

int main() {
  printf("%d\n", entry());

  return 0;
}
