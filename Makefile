#COpts = -std=c99 -O3
COpts = -std=c99 -g -DDEBUG -Wall
	
default: test_lizp test_core repl

test_lizp: src/test_lizp.c
	$(CC) $(COpts) -o $@ src/test_lizp.c

test_core: src/test_core.c
	$(CC) $(COpts) -o $@ src/test_core.c

repl: src/repl.c
	$(CC) $(COpts) -o $@ src/repl.c

clean:
	rm repl
	rm test_lizp
	rm test_core

