#!/bin/bash

ocamlc -c syntax.ml

ocamlc -c lexer.mli
ocamlc -c lexer.ml

ocamlc -c parser.mli
ocamlc -c parser.ml

ocamlc -c type.mli
ocamlc -c type.ml

ocamlc -c analyser.mli
ocamlc -c analyser.ml

ocamlc -c codegen.mli
ocamlc -c codegen.ml

ocamlc -c compiler.ml
