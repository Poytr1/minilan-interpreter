all:
	ghc --make Main.hs -o minilan

clean: 
	-rm -f *.dyn_hi *.dyn_o *.hi *.o

distclean: clean
	-rm -f Main.* Eval.* DataType.* Lexer.* Parser.* Rename.*
