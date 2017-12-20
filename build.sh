#!/bin/sh

if [ -z $1 ]
then
    TARGET=evolucell.byte
else
    TARGET=$1
fi

# Compile to bytecode
ocamlbuild -use-menhir -menhir "menhir --explain" -I src -pkg extlib $TARGET

# Translate to JavaScript

