
OPTIONS=-use-ocamlfind

all:
	ocamlbuild $(OPTIONS) src/unif.cma src/unif.cmxa

clean:
	ocamlbuild -clean

.PHONY: all clean
