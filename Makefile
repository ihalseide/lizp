CFLAGS = -g -Wall -Wextra -std=c99 -Wfatal-errors

repl: src/lizp.h src/repl.c
	cc $(CFLAGS) -o repl src/repl.c

read_print: src/lizp.h src/read_print.c
	cc $(CFLAGS) -o read_print src/read_print.c

config: src/lizp.h src/config.c
	cc $(CFLAGS) -o config src/config.c

lizp_test: src/lizp.h src/lizp_test.c
	cc $(CFLAGS) -o lizp_test src/lizp_test.c

