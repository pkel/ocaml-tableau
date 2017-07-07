.PHONY: run run_byte run_native main clean force

run: run_byte

run_byte: main.byte
	./main.byte

run_native: main.native
	./main.native

clean:
	rm -rf *.cmi *.cmo main.native main.byte _build

force: clean main

main: main.byte

main.byte:
	ocamlbuild main.byte

main.native:
	ocamlbuild main.native

.SECONDARY:

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml %.cmi
	ocamlc -c $<
