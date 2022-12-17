.PHONY: build
build:
	dune build bin/antar.exe
	cp _build/default/bin/antar.exe hcc

.PHONY: run
run:
	dune exec bin/antar.exe

.PHONY: test_all
test_all:
	dune runtest
	make clean

runtime/runtime.o: runtime/runtime.c

.PHONY: clean
clean:
	rm -f hcc
	rm -f a.out
	rm -f mygraph.dot
	rm -f mygraph.png
	rm -f runtime/runtime.o
	dune clean

# You can stick this section in your own project if you wish.
# 'make graph' produces a image that can be included in 'README.md'.
.PHONY: graph
graph: deps.png
deps.png:
	mkdir -p img
	dune-deps | tred | dot -Tpng > img/deps.png

.PHONY: utop
utop:
	dune utop src/

