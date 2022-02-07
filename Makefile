COpts = -std=c99 -Wall -O0 -g -pedantic

main: main.c
	gcc $(COpts) -o main main.c

run: main
	./main

