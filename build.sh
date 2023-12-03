#!/bin/bash

ocamlc -c syntax.ml

ocamlc -c scanner.ml
ocamlc -c scanner.mli

ocamlc -c parser.mli
ocamlc -c parser.ml

ocamlc -c type.mli
ocamlc -c type.ml

ocamlc -c analyser.mli
ocamlc -c analyser.ml

ocamlc -c codegen.mli
ocamlc -c codegen.ml

ocamlc syntax.cmo scanner.cmo parser.cmo type.cmo analyser.cmo codegen.cmo compiler.ml -o bmc
