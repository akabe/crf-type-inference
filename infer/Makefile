TARGET=infer
XOBJS=infer.cmx
PKGS=ppx_deriving.show,slap,slap.ppx,gsl
LDLIBS=str.cma dynlink.cma -I ../lib crflambda.cma
LDXLIBS=str.cmxa dynlink.cmxa -I ../lib crflambda.cmxa

.PHONY: all lib clean depend

all: $(TARGET)

$(TARGET): ../lib/crflambda.cma $(XOBJS)
	$(OCAMLOPT) -linkpkg $(XOBJS) -o $@

../lib/crflambda.cma:
	make -C ../lib

include ../common.mk
