CFLAGS=-ansi -pedantic -lX11 -lm -lSDL2 -I../c/raw

all: human

human: ../c/raw/tankode.o

test: all
	./human

clean:
	rm -f human
