.PHONY: run clean force
run: main
	./main

clean:
	rm -f *.cmi *.cmo main

force: clean main

main : formula.cmo prover.cmo main.cmo
	ocamlc -o $@ $^

.SECONDARY:

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml %.cmi
	ocamlc -c $<
