// 05-04-26 - C euler's totient; based on my Python implementation.
#include <assert.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

long phi(const long n) {
  assert(n >= 1);
  long result = n, remaining = n;

  if (remaining % 2 == 0) {
    result -= result / 2;
    while (remaining % 2 == 0) {
      remaining /= 2;
    }
  }

  long factor = 3;
  while (factor * factor <= remaining) {
    if (remaining % factor == 0) {
      result -= result / factor;
      while (remaining % factor == 0) {
        remaining /= factor;
      }
    }
    factor += 2;
  }

  if (remaining > 1) {
    result -= result / remaining;
  }

  return result;
}

int main(const int argc, const char *argv[]) {
  setlocale(LC_ALL, "");
  srand(time(NULL));
  long n = rand() % 1000 + 1;
  if (argc > 1)
    n = strtol(argv[1], NULL, 10);
  printf("%lc(%ld) = %ld\n", 0x03C6, n, phi(n));
}