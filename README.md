# StoryBook
A language of our own design with story-like syntax

Running examples
```
ocamllex scanner.mll
ocamlyacc -v parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c scanner.ml

menhir --interpret --interpret-show-cst parser.mly
FUNCTION ID LPAREN RPAREN RETURNS NUMBER LBRACE RBRACE EOF

```
