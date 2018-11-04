.PHONY: all

all:
	dune build @install -j auto

test:
	dune runtest

install:
	dune install

clean:
	dune clean
