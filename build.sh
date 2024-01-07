#!/bin/bash

ocamlc -c syntax.ml
if [ $? -ne 0 ]; then
    echo "Syntax did not compile"
    exit $?
fi

ocamlc -c parseLib.mli
ocamlc -c parseLib.ml
if [ $? -ne 0 ]; then
    echo "ParseLib did not compile"
    exit $?
fi

ocamlc -c scanner.mli
ocamlc -c scanner.ml
if [ $? -ne 0 ]; then
    echo "Scanner did not compile"
    exit $?
fi

ocamlc -c parsing.mli
ocamlc -c parsing.ml
if [ $? -ne 0 ]; then
    echo "Parser did not compile"
    exit $?
fi

ocamlc -c errorStateMonad.ml
if [ $? -ne 0 ]; then
    echo "ErrorStateMonad did not compile"
    exit $?
fi

ocamlc -c scopeTable.mli
ocamlc -c scopeTable.ml
if [ $? -ne 0 ]; then
    echo "Scope resolver did not compile"
    exit $?
fi

ocamlc -c scope.mli
ocamlc -c scope.ml
if [ $? -ne 0 ]; then
    echo "Scope resolver did not compile"
    exit $?
fi


ocamlc -c type.mli
ocamlc -c type.ml
if [ $? -ne 0 ]; then
    echo "Typechecker did not compile"
    exit $?
fi

ocamlc -c diffList.mli
ocamlc -c diffList.ml
if [ $? -ne 0 ]; then
    echo "Difference list module did not compile"
    exit $?
fi

ocamlc -c code.mli
ocamlc -c diffList.cmo code.ml
if [ $? -ne 0 ]; then
    echo "Codegen helper module did not compile"
    exit $?
fi

ocamlc -c x86Codegen.mli
ocamlc code.cmo -c x86Codegen.ml
if [ $? -ne 0 ]; then
    echo "Code generator did not compile"
    exit $?
fi

ocamlc syntax.cmo parseLib.cmo scanner.cmo parsing.cmo type.cmo errorStateMonad.cmo scopeTable.cmo scope.cmo diffList.cmo code.cmo x86Codegen.cmo compiler.ml -o bmc
if [ $? -ne 0 ]; then
    echo "Compiler application did not compile"
    exit $?
fi
gcc -c runtime.c
if [ $? -ne 0 ]; then
    echo "Runtime library did not compile"
    exit $?
fi
chmod +x bmc
