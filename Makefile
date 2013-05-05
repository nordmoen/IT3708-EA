all : main.hs selection.hs ea.hs evo.hs config.hs
	ghc --make -O -fllvm *.hs

clean: 
	rm -f main
	rm -f *.hi
	rm -f *.o
