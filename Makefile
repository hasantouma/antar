build:
	dune build bin/main.exe

run:
	dune exec bin/main.exe

test_all:
	dune runtest

.PHONY: clean

clean:
	dune clean


