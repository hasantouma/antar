build:
	dune build bin/main.exe

run:
	dune exec bin/main.exe

.PHONY: clean

clean:
	dune clean


