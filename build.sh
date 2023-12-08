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

ocamlc -c diffList.mli
ocamlc -c diffList.ml

ocamlc -c code.mli
ocamlc -c diffList.cmo code.ml

ocamlc -c x86Codegen.mli
ocamlc code.cmo -c x86Codegen.ml

ocamlc syntax.cmo scanner.cmo parsing.cmo type.cmo analyser.cmo diffList.cmo code.cmo x86Codegen.cmo compiler.ml -o bmc
gcc -c runtime.c
chmod +x bmc
