.PHONY: pack all run clean
GENERATED=src/parser.mli src/parser.ml src/lexer.ml
SOURSE=utils.ml equaler.ml checker.ml prover.ml
OCAMLC=ocamlc # ocamlopt for speed
PACKAGE=hw3.zip

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run: main.exe
	./main.exe   

compParser:
	cd src && $(OCAMLC) $(GENERATED:src/%=%)

compGrammar:
	cd src && $(OCAMLC) -c grammar.ml    

compSource:
	cd src && $(OCAMLC) -c $(SOURSE)
    
main.exe: $(GENERATED) compGrammar compParser compSource
	cd src && $(OCAMLC) grammar.ml $(GENERATED:src/%=%) $(SOURSE)  main.ml -o ../main.exe

pack: $(GENERATED)
	zip $(PACKAGE) -r Makefile src proofs

$(GENERATED): src/lexer.mll src/parser.mly
	ocamllex src/lexer.mll 
	ocamlyacc src/parser.mly

clean:
	cd src && $(DEL) $(GENERATED:src/%=%) *.c* *.o*
	$(DEL) main.exe $(PACKAGE)
