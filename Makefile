build:
	dune build bin/main.exe

run:
	dune exec bin/main.exe

test_all:
	dune runtest

.PHONY: clean

clean:
	dune clean

# You can stick this section in your own project if you wish.
# 'make graph' produces a image that can be included in 'README.md'.
#
.PHONY: graph deps.png
graph: deps.png
deps.png:
	mkdir -p img
	dune-deps | tred | dot -Tpng > img/deps.png

