# Makefile for Tankode's display
CFLAGS = -Wall -Wno-unused-function -Werror -Wno-deprecated-declarations
LDLIBS = -lGL -lGLU -lglut -lm -lalut -lopenal

BIN = bin/tankode-display

all: $(BIN)

bin/tankode-display: src/tankode-display
	mkdir -p bin
	cp src/tankode-display bin/tankode-display

src/tankode-display: src/draw.o src/io.o src/audio.o src/tankode-audio.o

src/io.o: src/io.c src/io.h src/tankode.h

src/draw.o: src/draw.c src/draw.h src/tankode.h

src/tankode-audio.o: src/tankode-audio.c src/tankode-audio.h

src/audio.o: src/audio.c src/audio.h

run: all
	./bin/tankode-display

clean:
	rm -f src/*.o $(BIN) src/tankode-display
