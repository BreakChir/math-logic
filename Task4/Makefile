.PHONY: pack all run clean
PACKAGE=hw4.zip

ifeq ($(OS),Windows_NT)
	DEL=del /f
else
	DEL=rm -f
endif

all: main.exe

run: main.exe
	./main.exe
    
main.exe:
	cd src && ocamlc str.cma main.ml -o ../main.exe

pack:
	zip $(PACKAGE) -r Makefile src

clean:
	cd src && $(DEL) *.c* *.o*
	$(DEL) main.exe $(PACKAGE)
