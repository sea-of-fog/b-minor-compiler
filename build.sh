#!/bin/bash

ocamlc -c syntax.ml

ocamlc -c scanner.mli
ocamlc -c scanner.ml

ocamlc -c parsing.mli
ocamlc -c parsing.ml

ocamlc -c type.mli
ocamlc -c type.ml

ocamlc -c analyser.mli
ocamlc -c analyser.ml

ocamlc -c x86Codegen.mli
ocamlc -c x86Codegen.ml

ocamlc syntax.cmo scanner.cmo parsing.cmo type.cmo analyser.cmo x86Codegen.cmo compiler.ml -o bmc
chmod +x bmc
