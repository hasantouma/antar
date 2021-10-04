build:
	dune build bin/rlang/antar_r1.exe
	ln -sf _build/default/bin/rlang/antar_r1.exe antar
	dune build bin/rlang/antar_r0.exe
	ln -sf _build/default/bin/rlang/antar_r0.exe antar-r0

run:
	dune exec bin/rlang/antar_r1.exe

test_all:
	make clean
	dune runtest
	make clean

.PHONY: clean

clean:
	rm -f antar
	rm -f antar-r0
	rm -f mygraph.dot
	rm -f mygraph.png
	dune clean

# You can stick this section in your own project if you wish.
# 'make graph' produces a image that can be included in 'README.md'.
#
.PHONY: graph deps.png
graph: deps.png
deps.png:
	mkdir -p img
	dune-deps | tred | dot -Tpng > img/deps.png

