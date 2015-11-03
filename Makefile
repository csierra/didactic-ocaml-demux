.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind main.native

clean:
	rm -rf main.byte main.native _build *~
