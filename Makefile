.PHONY: all clean

all:
	ocamlbuild -use-ocamlfind src/main.native

clean:
	rm -rf main.byte main.native _build *~
