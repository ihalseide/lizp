CC = clang
COpts = -std=c99 -g -Wall -Wextra

default: lizp

lizp: main.c lizp.o
	$(CC) $(COpts) -o $@ $^

%.o: %.c %.h
	$(CC) $(COpts) -c $^

clean:
	rm *.o
	rm *.gch
	rm lizp

