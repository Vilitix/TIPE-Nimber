EXEC=test

all: $(EXEC)



Nim_func.cmi: Nim_func.mli
	ocamlc -c $<

Nim_func.cmx: Nim_func.ml Nim_func.cmi
	ocamlopt -c $<

Kayles.cmi: Kayles.mli
	ocamlc -c $<

Kayles.cmx: Kayles.ml Kayles.cmi
	ocamlopt -c $<

Twopins.cmi: Twopins.mli
	ocamlc -c $<

Twopins.cmx: Twopins.ml Twopins.cmi
	ocamlopt -c $<

Marienbad_reso.cmi: Marienbad_reso.mli
	ocamlc -c $<

Marienbad_reso.cmx: Marienbad_reso.ml Marienbad_reso.cmi
	ocamlopt -c $<

Affichage_table_nimber.cmi: Affichage_table_nimber.mli
	ocamlc -c $<

Affichage_table_nimber.cmx: Affichage_table_nimber.ml Affichage_table_nimber.cmi
	ocamlopt -c $<

Union_find.cmi: Union_find.mli
	ocamlc -c $<

Union_find.cmx: Union_find.ml Union_find.cmi 
	ocamlc -c $<	

Projet_Cram.cmi: Projet_Cram.mli
	ocamlc -c $<

Projet_Cram.cmx: Projet_Cram.ml Projet_Cram.cmi 
	ocamlc -c $<




$(EXEC): Nim_func.cmx Kayles.cmx Twopins.cmx Marienbad_reso.cmx Affichage_table_nimber.cmx Union_find.cmx Projet_Cram.cmx test.ml
	ocamlopt -o $@ $^

clean:
	rm -rf *.o
	rm -rf *.cmi
	rm -rf *.cmo
	rm -rf *.cmx
	rm -rf $(EXEC)