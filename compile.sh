ocamlc -I +bitstring unix.cma bitstring.cma -pp "camlp4o -I /usr/lib/ocaml/bitstring bitstring.cma bitstring_persistent.cma pa_bitstring.cmo" main.ml
