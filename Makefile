#Note pour Luka, la première fois lancer : eval $(opam config env) make

binary:
	dune build main.exe

byte:
	dune build main.bc

clean:
	dune clean
