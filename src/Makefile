ifdef WITH_BIN_ANNOT
	OCAMLFLAGS += -bin-annot
endif

ifdef WITH_DEBUG
	OCAMLFLAGS += -g
	OCAMLLDFLAGS += -g
endif

RESULT = owee

SOURCES = owee_buf.mli owee_buf.ml owee_interval_tree.ml owee_interval_tree.mli \
  owee_elf.mli owee_elf.ml owee_debug_line.ml \
  owee_debug_line.mli owee_location.mli owee_location.ml owee_marker.mli \
  owee_marker.ml owee_stubs.c

PACKS = unix bigarray

all: byte-code-library native-code-library

-include OCamlMakefile
