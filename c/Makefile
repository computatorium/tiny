CC=gcc
CFLAGS=-Wall -Wextra -O2
LDLIBS=-leditline

SRCS=$(wildcard */*.c)
BINS=$(SRCS:.c=)

all: $(BINS)

%: %.c
	$(CC) $(CFLAGS) $< -o $@ $(LDLIBS)

clean:
	rm -f $(BINS)

.PHONY: all clean