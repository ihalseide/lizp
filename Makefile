CC = clang
#COpts = -std=c99 -O3
COpts = -std=c99 -g -DDEBUG -Wall
	
default: lizp

lizp: src/main.c lizp.o
	$(CC) $(COpts) -o lizp src/main.c lizp.o

test: src/test.c lizp.o
	$(CC) $(COpts) -o test src/test.c lizp.o

lizp.o: src/lizp.c src/lizp.h
	$(CC) $(COpts) -o lizp.o -c src/lizp.c

clean:
	rm lizp.o
	rm lizp
	rm test

