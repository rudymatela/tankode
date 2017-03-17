# Makefile for Tankode's display
CFLAGS = -Wall -Wno-unused-function -Werror
LDLIBS = -lGL -lGLU -lglut -lm

BIN = bin/tankode-display

all: $(BIN)

bin/tankode-display: src/tankode-display
	mkdir -p bin
	cp src/tankode-display bin/tankode-display

src/tankode-display: src/draw.o src/io.o

src/io.o: src/io.c src/io.h src/tankode.h

src/draw.o: src/draw.c src/draw.h src/tankode.h

run: all
	./bin/tankode-display

clean:
	rm -f src/*.o $(BIN) src/tankode-display
