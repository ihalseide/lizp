COpts = -std=c99 -Wall

all: debug main

debug: lizp.c
	gcc $(COpts) -o debug -g lizp.c

main: lizp.c
	gcc $(COpts) -o main lizp.c

run: main
	./main

