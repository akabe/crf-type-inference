XOBJS=model.cmx train.cmx
PKGS=ppx_deriving.show,slap,slap.ppx,gsl
LDLIBS=-I ../../lib crflambda.cma
LDXLIBS=-I ../../lib crflambda.cmxa

.PHONY: all lib clean depend

all: train.out model.cmxs

train.out: ../../lib/crflambda.cma $(XOBJS)
	$(OCAMLOPT) -linkpkg $(XOBJS) -o $@

model.cmxs: ../../lib/crflambda.cma model.cmx
	$(OCAMLOPT) -shared -linkpkg model.cmx -o $@

../../lib/crflambda.cma:
	make -C ../../lib

include ../../common.mk
