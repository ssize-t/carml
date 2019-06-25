.PHONY: build

build:
	dune build @install --profile release

format:
	dune build @fmt --auto-promote

test: build
	dune runtest -f

clean:
	dune clean