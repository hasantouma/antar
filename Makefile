build:
	dune build bin/antar.exe
	ln -sf _build/default/bin/antar.exe antar

run:
	dune exec bin/antar.exe

test_all: runtime/runtime.o
	dune runtest
	make clean

runtime/runtime.o: runtime/runtime.c

clean:
	rm -f antar
	rm -f mygraph.dot
	rm -f mygraph.png
	rm -f runtime/runtime.o
	dune clean

.PHONY: build run test_all clean graph
# You can stick this section in your own project if you wish.
# 'make graph' produces a image that can be included in 'README.md'.
#
graph: deps.png
deps.png:
	mkdir -p img
	dune-deps | tred | dot -Tpng > img/deps.png

