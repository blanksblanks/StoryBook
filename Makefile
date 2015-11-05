BJS = ast.cmo parser.cmo scanner.cmo 
TESTS = \
arith1 \
arith2 \
fib \
for1 \
func1 \
func2 \
func3 \
gcd \
gcd2 \
global1 \
hello \
if1 \
if2 \
if3 \
if4 \
ops1 \
var1 \
while1

# Choose one
YACC = ocamlyacc
# YACC = menhir --explain

TARFILES = Makefile scanner.mll parser.mly \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

all: scanner.ml parser.ml ast.cmo parser.cmo scanner.cmo ast.cmi parser.cmi scanner.cmi

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	$(YACC) -v parser.mly

ast.cmo : ast.ml
	ocamlc -c ast.ml

parser.cmo : parser.ml
	ocamlc -c parser.ml

scanner.cmo : scanner.ml
	ocamlc -c scanner.ml

ast.cmi : ast.ml
	ocamlc -c ast.ml

parser.cmi : parser.mli
	ocamlc -c parser.mli

scanner.cmi : scanner.ml
	ocamlc -c scanner.ml

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml \
	*.cmo *.cmi *.out *.diff

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
parser.cmi: ast.cmo 

