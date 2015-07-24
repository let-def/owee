all: decodedline

owee:
	make -C .. owee

decodedline: decodedline.ml owee
	ocamlopt -o $@ -g unix.cmxa bigarray.cmxa -I ../src ../src/owee.cmxa decodedline.ml

clean:
	rm -f *.cm* *.o *.a decodedline
