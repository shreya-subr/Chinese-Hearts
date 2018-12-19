MODULES=state card command main ai
OBJECTS=$(MODULES:=.cmo)
MAIN=main.byte
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) -package ANSITerminal

test:
	ocamlbuild -clean
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:	
	ocamlbuild -clean -package ANSIterminal
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN) 

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private .\.byte 