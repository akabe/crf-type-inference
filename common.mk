##
## Common variables
##

CMPFLAGS=-annot -bin-annot -g -w A-4-33-41-42-43-34-44-45 -safe-string -strict-sequence -strict-formats
OCAMLC=ocamlfind ocamlc -package $(PKGS) $(CMPFLAGS) $(LDLIBS)
OCAMLOPT=ocamlfind ocamlopt -package $(PKGS) $(CMPFLAGS) $(LDXLIBS)
OCAMLDEP=ocamldep -native
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

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
