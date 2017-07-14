#!/bin/sh

# Compile to bytecode
ocamlbuild -I src -pkg extlib evolucell.byte

# Translate to JavaScript

