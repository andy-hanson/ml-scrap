#
# Pure OCaml, no packages, no _tags, code in serveral directories
#

# bin-annot is required for Merlin and other IDE-like tools
# The -I flag introduces sub-directories to search for code

#OCB_FLAGS = -tag bin_annot -I src -I src/utilities -I src/compile -I src/compile/lex
# -I src: use the src directory
# -r: recursive (doesn't seem to work?)
OCB_FLAGS = -use-ocamlfind -I src -r -I src/compile -I src/compile/check -I src/compile/parse -I src/compile/parse/lex -I src/run -I src/util
MORE_OCB_FLAGS = -lflags -custom,c_src/add.o
OCB = 		ocamlbuild $(OCB_FLAGS) $(MORE_OCB_FLAGS)

all:		run

# TODO: this is stupid
clib:
			ocamlbuild -use-ocamlfind add.o -I c_src

run: byte
			./main.byte

clean:
			$(OCB) -clean

#native:
#			$(OCB) main.native

byte:		clib
			$(OCB) main.byte

#profile:
#			$(OCB) -tag profile main.native

#debug:
#			$(OCB) -tag debug main.byte

#.PHONY: 	all clean byte native profile debug test
