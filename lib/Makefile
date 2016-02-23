TARGET=crflambda.cmxa
XOBJS=misc.cmx lambda.cmx lambdaParser.cmx lambdaLexer.cmx simpleType.cmx crf.cmx optimize.cmx modelLoader.cmx utils.cmx
DEPS=lambdaParser.ml lambdaLexer.ml
PKGS=ppx_deriving.show,slap,slap.ppx,gsl

.PHONY: all clean depend

all: $(TARGET)

$(TARGET): $(XOBJS)
	$(OCAMLOPT) -a $^ -o $@

clean::
	rm -f lambdaParser.ml lambdaParser.mli lambdaParser.output lambdaLexer.ml

include ../common.mk
