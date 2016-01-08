# StoryBook

StoryBook is an object-oriented programming language targeted toward children. The language uses intuitive, story-like syntax and structure, making it easier for children to understand and use. The compiler is written in OCaml and the language compiles to C.

See StoryBook_LRM.pdf for Language Reference Manual.

## Running StoryBook

To run, just type `make` from the CLI.


To set up the StoryBook compiler (only do once):

1. `chmod +x ./storybook`
2. Append the StoryBook directory (you can get this with `pwd`) to the `PATH` variable in your `.bashrc` or `.zshrc` file. For example:
```
# StoryBook configuration
export PATH="$PATH:/Users/blanks/Development/StoryBook"
```

To run StoryBook code: `storybook HelloWorld.sbk`

You can `cat HelloWorld.c` to see the generated C code.

To remove the machine-generated files, just run `make clean`.

## Testing StoryBook

To run tests:

1. Go to test directory `cd test`
2. Make test script executable `chmod +x test.sh` (only do once)
3. Run all tests: `./test.sh`
4. See test results: `cat test_results.txt`

According to [Real World OCaml](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html), "Menhir is an alternative parser generator that is generally superior to the venerable ocamlyacc, which dates back quite a few years." Menhir allows you to convert a stream of tokens to an AST, as the example of an empty function declaration shows.
To run parser with menhir:
```
menhir --interpret --interpret-show-cst parser.mly
FUNCTION ID LPAREN RPAREN RETURNS NUMBER LBRACE RBRACE EOF
```
