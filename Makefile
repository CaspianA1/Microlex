OUT = demo_microlex
REMOVABLE = *.cmi *.cmx *.o

all: build
	./demo/$(OUT)

build:
	ocamlopt str.cmxa microlex.ml demo/$(OUT).ml -o demo/$(OUT)
	rm $(REMOVABLE); cd demo; rm $(REMOVABLE)