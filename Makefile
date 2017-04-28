
TGT := ofxparse
OCAMLC := ocamlc -annot -g -w Aez -warn-error A

$(TGT): dump.cmo parser.cmo lexer.cmo main.ml
	$(OCAMLC) -o $@ ast.ml dump.ml parser.ml lexer.ml \
	                currency.ml stmttrn.ml banktranlist.ml main.ml

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

dump.cmo dump.cmi: ast.cmi dump.ml
	$(OCAMLC) -c dump.ml

ast.cmo ast.cmi: ast.ml
	$(OCAMLC) -c ast.ml

parser.cmo parser.cmi: dump.cmi ast.cmi parser.ml
	$(OCAMLC) -c parser.mli parser.ml

lexer.cmo: parser.cmi lexer.ml
	$(OCAMLC) -c lexer.ml

.PHONY: clean test
clean:
	rm -f -- *.mli *.cmo *.cmi parser.ml lexer.ml $(TGT) a.out \
	         *~ *annot

test: $(TGT)
	./$(TGT) < test1.ofx
