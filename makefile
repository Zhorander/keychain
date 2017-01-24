FIND=ocamlfind
OC=ocamlopt
ENC=-package cryptokit
SQL=-package sqlite3
GTK=-package lablgtk2
GRAPH=-package graphics
GUI_LIB=/home/elias/Documents/Ocaml/graphics/gui_lib/

all: encrypt database keychain keychain_gui gui_menu

database: database.ml
	$(FIND) $(OC) -c $@.ml $(SQL) $(ENC)

encrypt: encrypt.ml
	$(FIND) $(OC) -c $@.ml $(ENC)

keychain: keychain.ml database.ml encrypt.ml
	$(FIND) $(OC) $(ENC) $(SQL) database.cmx encrypt.cmx $@.ml -o $@ -linkpkg

keychain_gui: keychain_gui.ml
	$(FIND) $(OC) $(GTK) $@.ml -o $@ -linkpkg

gui_menu: gui_menu.ml
	$(FIND) $(OC) -I $(GUI_LIB) $(GRAPH) buttons.cmx text_boxes.cmx $@.ml -o $@ -linkpkg

test: test.ml database.ml encrypt.ml
	$(FIND) $(OC) $(ENC) $(SQL) database.cmx encrypt.cmx $@.ml -o $@ -linkpkg

clean:
	rm *.cmi
	rm *.o
	rm *.cmx
