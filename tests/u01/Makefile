LIB_PATH=../../_build/install/default/lib/randii
OPAM_PATH=../../../_opam

.PHONY: run lib client clean

run:
	make lib
	make client
	LD_LIBRARY_PATH=$(LIB_PATH):$(OPAM_PATH)/lib \
	LIBRARY_PATH=$(OPAM_PATH)/lib \
	C_INCLUDE_PATH=$(OPAM_PATH)/include \
	./a.out

lib:
	dune build @install

client:
	$(CC) -I $(LIB_PATH) -I $(OPAM_PATH)/include -L $(LIB_PATH) -L $(OPAM_PATH)/lib main.c \
	    	-Wl,--no-as-needed -ldl -lm -lrandii-u01 -ltestu01

clean:
	dune clean
	rm -f a.out
