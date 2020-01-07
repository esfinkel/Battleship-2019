MODULES=command main author board ai_random ai_normal ai_smart custom_board_parser loading_screen

OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml) helpers.ml
MLIS=$(MODULES:=.mli) ai.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,oUnit,str,qcheck,ANSITerminal,Yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

build-full: build
	$(OCAMLBUILD) $(MAIN)

test: build
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play-outside:
	echo "not yet implemented"

play_h: build-full
	bash bg_music.sh && (killall afplay || echo "") && echo "log back in for your next war!\n"

# I think afplay is Mac-only?

# -s silence command printing
# -k ignore errors
play-music play_music:
	make -s -k play_h

play: build-full
	./$(MAIN) 0

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

bisect-view: bisect
	open report/index.html

zip: bisect
	zip -r battleship.zip *.txt *.ml* *.json bg_music.sh _tags Makefile report/ custom_boards/ .merlin .ocamlinit README.md	audio/

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS) helpers.ml

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report battleship.zip bisect*.out
