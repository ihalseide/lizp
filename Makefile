CFLAGS = -g -Wall -Wextra -std=c99 -Wfatal-errors
#CFLAGS = -std=c99 -Wfatal-errors

repl: src/lizp.h src/repl.c
	cc $(CFLAGS) -o repl src/repl.c

read_print: src/lizp.h src/read_print.c
	cc $(CFLAGS) -o read_print src/read_print.c

config: src/lizp.h src/config.c
	cc $(CFLAGS) -o config src/config.c

test_lizp: src/lizp.h src/test_lizp.c
	cc $(CFLAGS) -o test_lizp src/test_lizp.c

