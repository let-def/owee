all: owee

owee:
	$(MAKE) -C src
	
clean:
	$(MAKE) -C src $@

.PHONY: all owee clean install uninstall reinstall

DIST_FILES=               \
	src/owee_buf.mli        \
	src/owee_buf.ml         \
	src/owee_buf.cmi        \
	src/owee_buf.cmo        \
	src/owee_buf.cmx        \
	src/owee_elf.mli        \
	src/owee_elf.ml         \
	src/owee_elf.cmi        \
	src/owee_elf.cmo        \
	src/owee_elf.cmx        \
	src/owee_debug_line.ml  \
	src/owee_debug_line.mli \
	src/owee_debug_line.cmi \
	src/owee_debug_line.cmo \
	src/owee_debug_line.cmx \
	src/owee_location.mli   \
	src/owee_location.ml    \
	src/owee_location.cmi   \
	src/owee_location.cmo   \
	src/owee_location.cmx   \
	src/owee.cma            \
	src/owee.a							\
	src/owee.cmxa

$(DIST_FILES): owee

install: $(DIST_FILES) src/META
	ocamlfind install owee $^

uninstall:
	ocamlfind remove owee

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install
