CC = clang
COpts = -std=c99 -g -Wall -Wextra

default: lizp

lizp: main.c lizp_string.o cell.o function.o reader.o printer.o lizp.o env.o
	$(CC) $(COpts) -o $@ $^

%.o: %.c %.h
	$(CC) $(COpts) -c $^

clean:
	rm *.o
	rm *.gch
	rm lizp

