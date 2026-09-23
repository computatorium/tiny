// small key-value database console application
// stores strings, supports basic (de)serialization to/from a file

#include <editline/readline.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define INITIAL_CAPACITY 256

#include "table.h"

#include "command.h"

int main(void) {
  table_t *t = table_create(INITIAL_CAPACITY);
  if (!t)
    return 1;

  int interactive = isatty(STDIN_FILENO);
  if (interactive)
    fprintf(stderr, "kv: set k v, get k, del k, count, list, dump f, load f, quit\n");

  char *line;
  while ((line = readline(interactive ? "> " : NULL)) != NULL) {
    command_t c;
    command_init(&c);
    if (!c.argv) {
      free(line);
      continue;
    }
    const int r = parse_command(line, &c);

    if (r == 0) {
      command_free(&c);
      free(line);
      continue;
    }
    if (r < 0) {
      fprintf(stderr, "parse error\n");
      command_free(&c);
      free(line);
      continue;
    }

    char *cmd = c.argv[0];

    if (strcmp(cmd, "quit") == 0) {
      command_free(&c);
      free(line);
      break;
    } else if (strcmp(cmd, "set") == 0) {
      if (c.argc < 3) {
        fprintf(stderr, "usage: set <key> <value>\n");
      } else if (table_set(t, c.argv[1], c.argv[2]) < 0) {
        fprintf(stderr, "out of memory\n");
      }
    } else if (strcmp(cmd, "get") == 0) {
      if (c.argc < 2) {
        fprintf(stderr, "usage: get <key>\n");
      } else {
        char *v = table_get(t, c.argv[1]);
        if (v)
          printf("%s\n", v);
        else
          fprintf(stderr, "NOT FOUND\n");
      }
    } else if (strcmp(cmd, "del") == 0) {
      if (c.argc < 2) {
        fprintf(stderr, "usage: del <key>\n");
      } else if (!table_delete(t, c.argv[1])) {
        fprintf(stderr, "NOT FOUND\n");
      }
    } else if (strcmp(cmd, "count") == 0) {
      printf("%d\n", t->count);
    } else if (strcmp(cmd, "list") == 0) {
      table_foreach(t, print_entry);
    } else if (strcmp(cmd, "dump") == 0) {
      if (c.argc < 2) {
        fprintf(stderr, "usage: dump <file>\n");
      } else {
        FILE *f = fopen(c.argv[1], "w");
        if (!f) {
          fprintf(stderr, "failed to open file for writing\n");
        } else {
          table_serialize(t, f);
          fclose(f);
        }
      }
    } else if (strcmp(cmd, "load") == 0) {
      if (c.argc < 2) {
        fprintf(stderr, "usage: load <file>\n");
      } else {
        FILE *f = fopen(c.argv[1], "r");
        if (!f) {
          fprintf(stderr, "failed to open file for reading\n");
        } else {
          table_deserialize(t, f);
          fclose(f);
        }
      }
    } else {
      fprintf(stderr, "unknown command: %s\n", cmd);
    }

    command_free(&c);
    free(line);
  }

  table_free(t);
  return 0;
}