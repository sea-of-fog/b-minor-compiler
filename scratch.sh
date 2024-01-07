#!/bin/bash

ocamlc -c parsing.ml
if [ $? -ne 0 ]; then
    echo "Error"
fi
