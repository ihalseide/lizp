COpts = -std=c99 -Wall -O0 -g -pedantic

all: tags main

main: main.c
	gcc $(COpts) -o main main.c

tags: main.c
	ctags main.c

run: main
	./main

