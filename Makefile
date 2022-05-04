CC = clang
#COpts = -std=c99 -O3 -Wall
COpts = -std=c99 -g -Wall
SrcDir = src
BuildDir = objects
LizpMain = $(SrcDir)/main.c
TestMain = $(SrcDir)/test.c
	
default: lizp

lizp: $(LizpMain) $(BuildDir)/lizp.o
	$(CC) $(COpts) -o $@ $^

test: $(TestMain) $(BuildDir)/lizp.o
	$(CC) $(COpts) -o $@ $^

$(BuildDir)/%.o: $(SrcDir)/%.c $(SrcDir)/%.h | $(BuildDir)
	$(CC) $(COpts) -c -o $@ $<

$(BuildDir):
	mkdir -p $@

run: test lizp
	./test && ./lizp

clean:
	rm lizp
	rm test

