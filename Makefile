CFLAGS = -g -Wall -Werror -Wfatal-errors -O2

SRC := gen.c lex.c main.c parse.c type.c
OBJ := $(SRC:%.c=.build/%.o)

all: m

.build/%.o: %.c all.h
	@mkdir -p $(@D)
	$(CC) -o $@ -c $(CFLAGS) $<

m: $(OBJ)
	$(CC) -o $@ $(OBJ)

clean:
	rm -rf .build m

.PHONY: all clean setup
