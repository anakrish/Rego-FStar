#!/usr/bin/env bash

set -e

fstar.exe  --codegen OCaml --extract "Rego"  --odir out Rego.Value.fst
fstar.exe  --codegen OCaml --extract "Rego"  --odir out Rego.Main.fst
fstar.exe  --codegen OCaml --extract "Rego"  --odir out Rego.Ast.fst
fstar.exe  --codegen OCaml --extract "Rego"  --odir out Rego.Interpreter.fst

cd out
export OCAMLPATH="dirname $(which fstar.exe)"
ocamlbuild -use-ocamlfind -pkg batteries -pkg fstar.lib Rego_Main.native

./Rego_Main.native
