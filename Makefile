CC = gcc
COpts = -std=c99 -Wall -Wunused

default: lizp

lizp: main.c lizp.o cells.o
	$(CC) -o $@ $^

%.o: %.c %.h
	$(CC) $(COpts) -c $^

clean:
	rm *.o
	rm lizp

