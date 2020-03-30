all:
	happy -gca Samoyeet/Par.y
	alex -g Samoyeet/Lex.x
	ghc --make Samoyeet/Test.hs -o Samoyeet/Test

clean:
	-rm -f Samoyeet/*.log Samoyeet/*.aux Samoyeet/*.hi Samoyeet/*.o Samoyeet/*.dvi

distclean: clean
	-rm -f Samoyeet/Doc.* Samoyeet/Lex.* Samoyeet/Par.* Samoyeet/Layout.* Samoyeet/Skel.* Samoyeet/Print.* Samoyeet/Test.* Samoyeet/Abs.* Samoyeet/Test Samoyeet/ErrM.* Samoyeet/SharedString.* Samoyeet/ComposOp.* Samoyeet/samoyeet.dtd Samoyeet/XML.* Makefile*
		-rmdir -p Samoyeet/
