// 05-04-26 - crows nest
// from ch. 2 of "Tiny Python Projects" by Ken Youens-Clark

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

bool vowel(char c) {
  return isalpha(c) && strchr("aeiouAEIOU", c);
}

int usage(const char *progname) {
  fprintf(stderr, "Usage: %s <word>\n", progname);
  return 1;
}

int main(int argc, char *argv[]) {
  // display usage if no word is given, or if arg is -h or --help
  if (argc != 2 || strcmp(argv[1], "-h") == 0 ||
      strcmp(argv[1], "--help") == 0) {
    return usage(argv[0]);
  }

  printf("Ahoy, Captain, %s %s off the larboard bow!\n",
         vowel(argv[1][0]) ? "an" : "a", argv[1]);

  return 0;
}