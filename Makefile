CFLAGS = -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors -O2

SRC := $(wildcard *.c)
OBJ := $(SRC:%.c=.build/%.o)

all: setup m

setup:
	mkdir -p .build

.build/%.o: %.c
	$(CC) -o $@ -c $(CFLAGS) $<

m: $(OBJ)
	$(CC) -o $@ $(OBJ)

clean:
	rm -rf .build m

.PHONY: all clean setup
