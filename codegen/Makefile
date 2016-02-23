TARGET=codegen.out
XOBJS=type.cmx codegen.cmx
DEPS=
PKGS=ppx_deriving.show,slap,slap.ppx,gsl
LDLIBS=-I ../lib crflambda.cma
LDXLIBS=-I ../lib crflambda.cmxa

.PHONY: all clean depend

all: $(TARGET)

$(TARGET): $(XOBJS)
	$(OCAMLOPT) -linkpkg $^ -o $@

include ../common.mk
