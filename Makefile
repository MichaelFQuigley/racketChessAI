all: $(wildcard src/*.rkt)
	raco make src/chess_view.rkt

clean:
	rm -rf src/compiled

.PHONY: all clean
