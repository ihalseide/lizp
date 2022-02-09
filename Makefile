COpts = -std=c99 -Wall -O0 -g -pedantic

all: tags main

main: lizp.c
	gcc $(COpts) -o main lizp.c

tags: lizp.c
	ctags lizp.c

run: main
	./main

