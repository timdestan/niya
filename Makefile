all: niya.native

niya.native:
	rebuild niya.native

.PHONY: clean

clean:
	rm -rf niya.native _build/
