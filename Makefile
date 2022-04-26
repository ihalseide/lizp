CC = clang
COpts = -std=c99 -g -Wall
SrcDir = src
BuildDir = objects
LizpMain = $(SrcDir)/main.c
TestMain = $(SrcDir)/test.c
LizpSrc = lizp.c sequence.c printer.c reader.c value.c eval.c
TestSrc = lizp.test.c sequence.test.c reader.test.c printer.test.c value.test.c
LizpObjs = $(addprefix $(BuildDir)/,$(LizpSrc:.c=.o))
TestObjs = $(addprefix $(BuildDir)/,$(TestSrc:.c=.o))
	
default: test lizp

run: test lizp
	./test && ./lizp

lizp: $(LizpMain) $(LizpObjs)
	$(CC) $(COpts) -o $@ $^

test: $(TestMain) $(LizpObjs) $(TestObjs)
	$(CC) $(COpts) -o $@ $^

$(BuildDir)/%.o: $(SrcDir)/%.c $(SrcDir)/%.h | $(BuildDir)
	$(CC) $(COpts) -c -o $@ $<

$(BuildDir):
	mkdir -p $@

clean:
	rm $(LizpObjs)
	rm $(TestObjs)
	rm lizp
	rm test

