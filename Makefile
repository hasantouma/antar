build:
	dune build bin/antar.exe
	cp _build/default/bin/antar.exe hcc

run:
	dune exec bin/antar.exe

test_all:
	dune runtest
	make clean

runtime/runtime.o: runtime/runtime.c

clean:
	rm -f hcc
	rm -f a.out
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

