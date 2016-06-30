# Disabled warnings: See http://caml.inria.fr/pub/docs/manual-ocaml/comp.html
OCB_FLAGS = -use-ocamlfind -cflags -strict-sequence,-strict-formats,-nolabels,-w=A-4-28-29,-warn-error=A-32
OCB_BYTE_FLAGS = -lflags -custom,c_src/add.o
# TODO: http://caml.inria.fr/pub/docs/manual-ocaml/native.html#sec284
OCB_NATIVE_FLAGS = -cflags -cc=clang
OCB_NATIVE_SLOW_FLAGS = -cflags -g
OCB_NATIVE_FAST_FLAGS = -cflags -unsafe,-noassert,-no-app-funct
OCB = ocamlbuild $(OCB_FLAGS)

all:		run

# TODO: this is stupid, do it as part of compilation
clib:
	ocamlbuild -use-ocamlfind add.o -I c_src

run: byte
	./main.byte

run-native: native
	./main.native

run-fast: native-fast
	./main.native

clean:
	$(OCB) -clean

byte:		clib
	$(OCB) $(OCB_BYTE_FLAGS) main.byte

native: clib
	$(OCB) $(OCB_NATIVE_FLAGS) $(OCB_NATIVE_SLOW_FLAGS) main.native

native-fast: clib
	$(OCB) $(OCB_NATIVE_FLAGS) $(OCB_NATIVE_FAST_FLAGS) main.native

test: build-test run-test
build-test: clib
	$(OCB) $(OCB_BYTE_FLAGS) test.byte
run-test: clib
	./test.byte

profile: clib
	$(OCB) -tag profile main.native

debug: clib
	$(OCB) -tag debug main.byte

wc:
	wc `find src -name '*.ml*'`

.PHONY: 	all clean byte native profile debug test
