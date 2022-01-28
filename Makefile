COpts = -std=c99 -Wall -O0 -g

main: main.c
	gcc $(COpts) -o main main.c

run: main
	./main

