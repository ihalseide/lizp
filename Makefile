COpts = -std=c99 -Wall -O0 -g

main: main.c types.h
	gcc $(COpts) -o main main.c

run: main
	./main

