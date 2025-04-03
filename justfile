@format:
	@dune build @fmt --display=quiet --auto-promote;

@deps:
	@odep dune | dot -Tsvg > dune-odep.svg
