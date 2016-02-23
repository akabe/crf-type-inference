SUBDIRS=lib codegen infer $(shell find exp -maxdepth 1 -mindepth 1)

.PHONY: all clean depend dist

all:
	@for d in $(SUBDIRS); do \
	  make all -C $$d || exit 1; \
	done

clean:
	@for d in $(SUBDIRS); do \
	  make clean -C $$d || exit 1; \
	done

depend:
	@for d in $(SUBDIRS); do \
	  make depend -C $$d || exit 1; \
	done

dist: clean
	cd ..; \
	  rm -f mltyinf.tar.xz; \
	  tar -Jcvf mltyinf.tar.xz mltyinf
