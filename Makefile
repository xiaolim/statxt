statxt: hello.native
	ocamlbuild hello.native

.PHONY:
clean:
	rm -rf a.out
	rm -rf *.o
	
.PHONY:
all: clean statxt
