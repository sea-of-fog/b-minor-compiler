#!/bin/bash

ocamlc -c syntax.ml

ocamlc -c scanner.mli
ocamlc -c scanner.ml

ocamlc -c parser.mli
ocamlc -c parser.ml

ocamlc -c type.mli
ocamlc -c type.ml

ocamlc -c analyser.mli
ocamlc -c analyser.ml

ocamlc -c x86-codegen.mli
ocamlc -c x86-codegen.ml

ocamlc syntax.cmo scanner.cmo parser.cmo type.cmo analyser.cmo x86Codegen.cmo compiler.ml -o bmc
chmod +x bmc
