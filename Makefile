CC = clang
COpts = -std=c99 -g -Wall

default: lizp

lizp: main.c lizp.o sequence.o printer.o reader.o value.o eval.o
	$(CC) $(COpts) -o $@ $^

%.o: %.c %.h
	$(CC) $(COpts) -c $^

clean:
	rm *.o
	rm *.gch
	rm lizp

