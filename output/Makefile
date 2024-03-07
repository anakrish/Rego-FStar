FSTAR_HOME=$(realpath $(shell dirname $(shell which fstar.exe))/..)

ifeq ($(OS),Windows_NT)
  OCAMLPATH := $(shell cygpath -m $(FSTAR_HOME)/lib);$(OCAMLPATH)
else
  OCAMLPATH := $(FSTAR_HOME)/lib:$(OCAMLPATH)
endif
export OCAMLPATH

# with dune
Rego_Main.exe: $(wildcard *.ml)
	OCAMLPATH="$(OCAMLPATH)" dune build
	cp _build/default/Rego_Main.exe $@

#with ocamlbuild
# Main.native: $(wildcard *.ml)
# 	OCAMLPATH="$(OCAMLPATH)" ocamlbuild -tag thread -use-ocamlfind -quiet -pkg batteries -pkg fstar.lib -cflags -w,-8 Main.native

clean:
	rm -rf _build Rego_Main.native Rego_Main.exe
