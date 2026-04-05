// 05-04-26 - picnic
// from ch. 3 of "Tiny Python Projects" by Ken Youens-Clark

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// pstrcmp is strcmp wrapped for qsort(4)
static int pstrcmp(const void *a, const void *b) {
  const char *const *sa = a;
  const char *const *sb = b;
  return strcmp(*sa, *sb);
}

// this is just a helper for exiting early
static int usage(const char *progname, int code) {
  fprintf(stderr, "usage: %s [-s||--sorted] [-h||--help] str [str...]\n",
          progname);
  return code;
}

int main(const int argc, const char *argv[]) {
  if (argc < 2)
    return usage(argv[0], 1);

  int sort = 0;
  const char **items = malloc(argc * sizeof(*items));
  size_t len = 0;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-s") == 0 || strcmp(argv[i], "--sorted") == 0) {
      sort = 1;
    } else if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
      free(items);
      return usage(argv[0], 0);
    } else {
      items[len++] = argv[i];
    }
  }

  if (len == 0) {
    free(items);
    return usage(argv[0], 1);
  }

  if (sort) {
    qsort(items, len, sizeof(*items), pstrcmp);
  }

  printf("You are bringing ");

  switch (len) {
  case 1:
    // '<item>'
    printf("%s", items[0]);
    break;
  case 2:
    // '<item1> and <item2>'
    printf("%s and %s", items[0], items[1]);
    break;
  default:
    // '<item1>, <item2>, ... and <itemN>'
    for (size_t i = 0; i < len; i++) {
      if (i == len - 1)
        printf("and %s", items[i]);
      else
        printf("%s, ", items[i]);
    }
  }

  printf(".\n");

  free(items);

  return 0;
}
