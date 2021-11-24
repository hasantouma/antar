build:
	dune build bin/rlang/antar_r1.exe
	ln -sf _build/default/bin/rlang/antar_r1.exe antar-r1
	dune build bin/rlang/antar_r0.exe
	ln -sf _build/default/bin/rlang/antar_r0.exe antar-r0

run:
	dune exec bin/rlang/antar_r1.exe

test_all: runtime/runtime.o
	dune runtest
	make clean

runtime/runtime.o: runtime/runtime.c

clean:
	rm -f antar-r1
	rm -f antar-r0
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

