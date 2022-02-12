CC = gcc
COpts = -std=c99 -Wall -Wunused

default: main

all: debug main

debug: lizp.c
	$(CC) $(COpts) -o d_lizp -g lizp.c

main: lizp.c
	$(CC) $(COpts) -o m_lizp lizp.c

