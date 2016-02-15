#!/bin/bash

cat \
"libraries/either.yaspl" \
"libraries/maybe.yaspl" \
"libraries/list.yaspl" \
"libraries/bytes.yaspl" \
"libraries/io.yaspl" \
"libraries/numbers.yaspl" \
"libraries/lexer.yaspl" \
"libraries/sexp-parser.yaspl" \
"libraries/arithmetic-expr.yaspl" \
"libraries/tuples.yaspl" \
"libraries/join-list.yaspl" \
"libraries/dict.yaspl" \
"libraries/stack-machine.yaspl" \
"libraries/prim-implementation.yaspl" \
"libraries/x86-64-stack-machine.yaspl" \
"libraries/source-language.yaspl" \
"libraries/source-to-stack.yaspl" \
"libraries/compiler.yaspl" \
"libraries/main.yaspl"
