# StoryBook
A language of our own design with story-like syntax

To run, just type `make` from the CLI. It will execute the following commands:
```
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamllex scanner.mll
124 states, 6796 transitions, table size 27928 bytes
ocamlc -c scanner.ml
ocamlc -c sast.ml
ocamlc -c semantic_analyzer.ml
ocamlc -c codegen.ml
```

To run StoryBook code:
```./runall.sh HelloWorld.sbk```

You can ```cat HelloWorld.c``` to see the generated C code.

To remove the machine-generated files, just run `make clean`.

According to (Real World OCaml)[https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html], "Menhir is an alternative parser generator that is generally superior to the venerable ocamlyacc, which dates back quite a few years." Menhir allows you to convert a stream of tokens to an AST, as the example of an empty function declaration shows.
To run parser with menhir:
```
menhir --interpret --interpret-show-cst parser.mly
FUNCTION ID LPAREN RPAREN RETURNS NUMBER LBRACE RBRACE EOF
```
