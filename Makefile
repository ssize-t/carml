.PHONY: build

build:
	dune build @install

format:
	dune build @fmt --auto-promote

test: build
	dune runtest -f

clean:
	dune clean