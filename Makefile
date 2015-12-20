OBJS = parser.cmo scanner.cmo semantic_analyzer.cmo ast.cmo sast.cmo cast.cmo codegen.cmo pretty_print.cmo

# TARFILES = Makefile scanner.mll parser.mly \
# 	$(TESTS:%=tests/test-%.mc) \
# 	$(TESTS:%=tests/test-%.out)


run : $(OBJS)
	ocamlc -o run str.cma $(OBJS)

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<


# scanner.ml : scanner.mll
# 	ocamllex scanner.mll

# parser.ml parser.mli : parser.mly
# 	$(YACC) -v parser.mly

# semantic_analyzer.cmo : semantic_analyzer.ml
# 	ocamlc -o semantic_analyzer.ml

# sast.cmo : sast.ml
# 	ocamlc -c sast.ml

# ast.cmo : ast.ml
# 	ocamlc -c ast.ml

# parser.cmo : parser.ml
# 	ocamlc -c parser.ml

# scanner.cmo : scanner.ml
# 	ocamlc -c scanner.ml

# codegen.cmo : codegen.ml
# 	ocamlc -c codegen.ml

# ast.cmi : ast.ml
# 	ocamlc -c ast.ml

# semantic_analyzer.cmi : semantic_analyzer.mli
# 	ocamlc -c semantic_analyzer.mli

# parser.cmi : parser.mli
# 	ocamlc -c parser.mli

# scanner.cmi : scanner.mli
# 	ocamlc -c scanner.mli

# codegen.cmi : codegen.mli
# 	ocamlc -c codegen.mli

.PHONY : clean
clean :
	rm -f test/*.c test/*Out.txt test/test_results.txt test/errors.txt
	rm -f parser.ml parser.mli scanner.ml
	rm -f test/tree/test_results.txt
	rm -f *.cmo *.cmi *.out *.diff run
	rm -rf *.dSYM
# Generated by ocamldep *.ml *.mli
semantic_analyzer.cmo : sast.cmo ast.cmo
semantic_analyzer.cmx : sast.cmx ast.cmx
code_gen.cmo : sast.cmo
code_gen.cmx : sast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
run.cmo : scanner.cmo sast.cmo parser.cmi codegen.cmo ast.cmo semantic_analyzer.cmo
run.cmx : scanner.cmx sast.cmx parser.cmx codegen.cmx ast.cmx semantic_analyzer.cmx
sast.cmo : ast.cmo
sast.cmx : ast.cmx
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
parser.cmi : ast.cmo


