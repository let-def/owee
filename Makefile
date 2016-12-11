all: owee

owee:
	$(MAKE) -C src
	
clean:
	$(MAKE) -C src $@
	$(MAKE) -C examples $@

.PHONY: all owee clean install uninstall reinstall

DIST_FILES=               \
	src/owee_buf.mli        \
	src/owee_buf.ml         \
	src/owee_buf.cmi        \
	src/owee_buf.cmx        \
	src/owee_elf.mli        \
	src/owee_elf.ml         \
	src/owee_elf.cmi        \
	src/owee_elf.cmx        \
	src/owee_debug_line.ml  \
	src/owee_debug_line.mli \
	src/owee_debug_line.cmi \
	src/owee_debug_line.cmx \
	src/owee_location.mli   \
	src/owee_location.ml    \
	src/owee_location.cmi   \
	src/owee_location.cmx   \
	src/owee_macho.mli      \
	src/owee_macho.ml       \
	src/owee_macho.cmi      \
	src/owee_macho.cmx      \
	src/owee_marker.mli     \
	src/owee_marker.ml      \
	src/owee_marker.cmi     \
	src/owee_marker.cmx     \
	src/owee.cma            \
	src/owee.a							\
	src/owee.cmxa           \
	src/dllowee_stubs.so    \
	src/libowee_stubs.a

$(DIST_FILES): owee

install: $(DIST_FILES) src/META
	ocamlfind install owee $^

uninstall:
	ocamlfind remove owee

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

examples:
	$(MAKE) -C examples
