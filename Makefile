MODULES=command main author board test ai_random ai_normal ai_smart helpers

OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=unix,oUnit,str,qcheck

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

# $(OCAMLBUILD) $(MAIN) && ./$(MAIN)
# ((while :; do afplay audio/boom1.mp3; done) || echo "(end music)") & ($(OCAMLBUILD) $(MAIN) && ./$(MAIN) && killall afplay)
# what does "test -something afplay file" do?
play_h: build
	$(OCAMLBUILD) $(MAIN) && bash bg_music.sh && (killall afplay || echo "") && echo "log in during your next war!\n"
	
# play music                 (if exception vv)   & in parallel,     build + run         stop music after ocaml program ends 
#  ((afplay audio/s1.m4a || echo "")             & ($(OCAMLBUILD) $(MAIN) && ./$(MAIN) && killall afplay)) || (killall afplay || echo "")
# I think afplay is Mac-only?
# maybe I could write a Python script that is system-independent, and call it from here


# -s silence command printing
# -k ignore errors
play-music play_music: build
	make -s -k play_h

play: build
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

bisect: clean test
	bisect-ppx-report -I _build -html report bisect0001.out

zip: bisect
	zip battleship.zip *.txt *.ml* _tags Makefile report/*
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report battleship.zip bisect*.out
