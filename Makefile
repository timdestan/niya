all: niya.native

niya.native:
	ocamlbuild niya.native

.PHONY: clean

clean:
	rm -rf niya.native _build/
