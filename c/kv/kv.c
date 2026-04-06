// small key-value database console application
// stores strings
// TODO: persistence

#include <editline/readline.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define INITIAL_CAPACITY 256

unsigned int hash(const char *key, int capacity) {
  // djb2
  unsigned long h = 5381;
  int c;
  while ((c = (unsigned char)*key++))
    h = (h << 5) + h + c;
  return h % capacity;
}

typedef struct entry {
  char *key;
  char *value;
  struct entry *next;
} entry_t;

typedef struct table {
  entry_t **buckets;
  int capacity;
  int count;
} table_t;

table_t *table_create(const int capacity) {
  table_t *t = malloc(sizeof(table_t));
  if (!t)
    return NULL;
  t->capacity = capacity;
  t->count = 0;
  t->buckets = calloc(capacity, sizeof(entry_t *));
  if (!t->buckets) {
    free(t);
    return NULL;
  }
  return t;
}

void table_resize(table_t *t, const int capacity) {
  entry_t **new_buckets = calloc(capacity, sizeof(entry_t *));
  if (!new_buckets)
    return; // keep existing table intact

  for (int i = 0; i < t->capacity; i++) {
    entry_t *e = t->buckets[i];
    while (e) {
      entry_t *next = e->next;
      unsigned int h = hash(e->key, capacity);
      e->next = new_buckets[h];
      new_buckets[h] = e;
      e = next;
    }
  }

  free(t->buckets);
  t->buckets = new_buckets;
  t->capacity = capacity;
}

int table_set(table_t *t, const char *key, const char *value) {
  const unsigned int h = hash(key, t->capacity);

  entry_t *e = t->buckets[h];
  while (e) {
    if (strcmp(e->key, key) == 0) {
      char *new_value = strdup(value);
      if (!new_value)
        return -1;
      free(e->value);
      e->value = new_value;
      return 0;
    }
    e = e->next;
  }

  entry_t *new = malloc(sizeof(entry_t));
  if (!new)
    return -1;
  new->key = strdup(key);
  if (!new->key) {
    free(new);
    return -1;
  }
  new->value = strdup(value);
  if (!new->value) {
    free(new->key);
    free(new);
    return -1;
  }
  new->next = t->buckets[h];
  t->buckets[h] = new;
  t->count++;
  if (t->count * 4 > t->capacity * 3)
    table_resize(t, t->capacity * 2);
  return 0;
}

char *table_get(const table_t *t, const char *key) {
  const unsigned int h = hash(key, t->capacity);
  const entry_t *e = t->buckets[h];
  while (e) {
    if (strcmp(e->key, key) == 0)
      return e->value;
    e = e->next;
  }
  return NULL;
}

int table_delete(table_t *t, const char *key) {
  const unsigned int h = hash(key, t->capacity);
  entry_t *e = t->buckets[h], *prev = NULL;
  while (e) {
    if (strcmp(e->key, key) == 0) {
      if (prev)
        prev->next = e->next;
      else
        t->buckets[h] = e->next;
      free(e->key);
      free(e->value);
      free(e);
      t->count--;
      int new_cap = t->capacity / 2;
      if (new_cap < INITIAL_CAPACITY)
        new_cap = INITIAL_CAPACITY;
      if (t->count * 4 < t->capacity && t->capacity > INITIAL_CAPACITY)
        table_resize(t, new_cap);
      return 1;
    }
    prev = e;
    e = e->next;
  }
  return 0;
}

void table_foreach(const table_t *t,
                   void (*fn)(const char *key, const char *value)) {
  for (int i = 0; i < t->capacity; i++) {
    const entry_t *e = t->buckets[i];
    while (e) {
      fn(e->key, e->value);
      e = e->next;
    }
  }
}

void table_free(table_t *t) {
  for (int i = 0; i < t->capacity; i++) {
    entry_t *e = t->buckets[i];
    while (e) {
      entry_t *next = e->next;
      free(e->key);
      free(e->value);
      free(e);
      e = next;
    }
  }
  free(t->buckets);
  free(t);
}

void print_entry(const char *key, const char *value) {
  printf("%s: %s\n", key, value);
}

typedef struct command {
  int argc;
  int cap;
  char **argv;
} command_t;

void command_init(command_t *c) {
  c->argc = 0;
  c->cap = 8;
  c->argv = malloc(c->cap * sizeof(char *));
}

int command_push(command_t *c, char *arg) {
  if (c->argc >= c->cap) {
    int new_cap = c->cap * 2;
    char **new_argv = realloc(c->argv, new_cap * sizeof(char *));
    if (!new_argv)
      return -1;
    c->argv = new_argv;
    c->cap = new_cap;
  }
  c->argv[c->argc++] = arg;
  return 0;
}

void command_free(command_t *c) {
  free(c->argv);
  c->argv = NULL;
  c->argc = 0;
  c->cap = 0;
}

// parse_command: splits line into tokens, supports double-quoted strings.
// modifies the line in place. returns 1 if tokens found, 0 if empty, -1 on
// error.
int parse_command(char *line, command_t *out) {
  out->argc = 0;
  char *p = line;

  while (*p) {
    while (*p == ' ')
      p++;
    if (*p == '\0')
      break;

    if (*p == '"') {
      // quoted token: read until closing quote
      p++; // skip opening quote
      char *start = p;
      char *w = p; // write pointer for unescaping
      while (*p && *p != '"') {
        if (*p == '\\' && p[1] == '"') {
          *w++ = '"';
          p += 2;
        } else if (*p == '\\' && p[1] == '\\') {
          *w++ = '\\';
          p += 2;
        } else {
          *w++ = *p++;
        }
      }
      if (*p != '"')
        return -1; // unterminated quote
      *w = '\0';
      p++; // skip closing quote
      if (command_push(out, start) < 0)
        return -1;
    } else {
      // unquoted token
      char *start = p;
      while (*p && *p != ' ')
        p++;
      if (*p)
        *p++ = '\0';
      if (command_push(out, start) < 0)
        return -1;
    }
  }

  return out->argc > 0;
}

int main(void) {
  table_t *t = table_create(INITIAL_CAPACITY);
  if (!t)
    return 1;

  int interactive = isatty(STDIN_FILENO);
  if (interactive)
    fprintf(stderr, "kv: set k v, get k, del k, count, list, quit\n");

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
    } else {
      fprintf(stderr, "unknown command: %s\n", cmd);
    }

    command_free(&c);
    free(line);
  }

  table_free(t);
  return 0;
}