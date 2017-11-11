all: niya.native

niya.native: niya.ml
	ocamlbuild niya.native

.PHONY: clean

clean:
	rm -rf niya.native _build/
