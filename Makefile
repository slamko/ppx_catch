INPUT=test/test.ml
OUTPUT=test/check.ml

.PHONY: test
.SILENT: test

test: _build/default/test/pp.exe
	dune build
	$^ --impl $(INPUT) -o $(OUTPUT)
	_build/default/test/mppx.exe

build:
	dune build

clean:
	dune clean
