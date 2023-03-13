LIB_PATH=../../_build/install/default/lib/randii

.PHONY: run lib client clean

run:
	make lib
	make client
	LD_LIBRARY_PATH=$(LIB_PATH) ./a.out

lib:
	dune build @install

client:
	$(CC) -I $(LIB_PATH) -L $(LIB_PATH) main.c \
	    	-Wl,--no-as-needed -ldl -lm -lrandii-u01

clean:
	dune clean
	rm -f a.out
