CC = clang
COpts = -std=c99 -g -Wall

default: lizp

test: test.c lizp.test.o sequence.test.o reader.test.o printer.test.o lizp.o sequence.o printer.o reader.o value.o eval.o
	$(CC) $(COpts) -o $@ $^

lizp: main.c lizp.o sequence.o printer.o reader.o value.o eval.o
	$(CC) $(COpts) -o $@ $^

%.o: %.c %.h
	$(CC) $(COpts) -c $^

exe: test lizp cleanbin

cleanbin:
	rm *.o
	rm *.gch

clean: cleanbin
	rm lizp
	rm test

