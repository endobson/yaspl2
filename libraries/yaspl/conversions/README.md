# Conversion steps

* Raw Bytes
* -> sexp parser
* -> parse-module
* -> resolve-imports
* -> resolve-types
* -> expand-datatypes
* -> type-checker (Not yet moved)
* -> resolve-vars
* -> expand-varargs
* -> expand-patterns
* -> source to intermediate
* -> optimize intermediate
* -> intermediate to register
