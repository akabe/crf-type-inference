BYTE=infer.byte
TARGET=js/infer.js
OBJS=misc.cmo lambda.cmo lambdaParser.cmo lambdaLexer.cmo simpleType.cmo crf.cmo infer.ml
DEPS=lambda.ml simpleType.ml lambdaParser.mly lambdaLexer.mll lambdaParser.ml lambdaLexer.ml
PKGS=ppx_deriving.show,lwt,js_of_ocaml,js_of_ocaml.ppx

all: $(TARGET)

$(TARGET): $(BYTE)
	$(JS_OF_OCAML) $^ -o $@

$(BYTE): $(OBJS)
	$(OCAMLC) -linkpkg $^ -o $@

clean::
	rm -f $(TARGET) $(BYTE) $(DEPS) lambdaParser.mli lambdaParser.output

lambda.ml: ../lib/lambda.ml
	cp $^ $@

simpleType.ml: ../lib/simpleType.ml
	cp $^ $@

lambdaParser.mly: ../lib/lambdaParser.mly
	cp $^ $@

lambdaLexer.mll: ../lib/lambdaLexer.mll
	cp $^ $@

##
## Common variables
##

CMPFLAGS=-annot -bin-annot -g -w A-4-33-41-42-43-34-44-45 -safe-string -strict-sequence -strict-formats
OCAMLC=ocamlfind ocamlc -package $(PKGS) $(CMPFLAGS) $(LDLIBS)
OCAMLOPT=ocamlfind ocamlopt -package $(PKGS) $(CMPFLAGS) $(LDXLIBS)
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
JS_OF_OCAML=js_of_ocaml

##
## Common build rules
##

.PHONY: all clean depend

clean::
	rm -f *.cm* *.annot *.o *.a *.out

depend: $(DEPS)
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend

.SUFFIXES: .mly .mll .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<
