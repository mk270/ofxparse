
TGT := ofxparse
OCAMLC := ocamlc -annot -g -w Aez -warn-error A

$(TGT): eval.cmo parser.cmo lexer.cmo main.ml
	$(OCAMLC) -o $@ ast.ml eval.ml parser.ml lexer.ml main.ml

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

eval.cmo eval.cmi: ast.cmi eval.ml
	$(OCAMLC) -c eval.ml

ast.cmo ast.cmi: ast.ml
	$(OCAMLC) -c ast.ml

parser.cmo parser.cmi: eval.cmi ast.cmi parser.ml
	$(OCAMLC) -c parser.mli parser.ml

lexer.cmo: parser.cmi lexer.ml
	$(OCAMLC) -c lexer.ml

.PHONY: clean test
clean:
	rm -f -- *.mli *.cmo *.cmi parser.ml lexer.ml $(TGT) a.out \
	         *~ *annot

test: $(TGT)
	./$(TGT) < test1.ofx
