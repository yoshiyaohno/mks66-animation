default: main
	@echo
	@echo rendering animation...
	./mdl spinny.mdl

main: Main.hs Parser.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs \
		Lighting.hs Lexer.hs Interpret.hs
	ghc -dynamic -O2 Main.hs -o mdl

Parser.hs: mdl.y
	happy -o Parser.hs -g mdl.y

Lexer.hs: mdl.x
	alex -o Lexer.hs -g mdl.x

clean:
	rm *.hi *.o Parser.hs Lexer.hs mdl .tempimg.ppm

imgclean:
	rm *.ppm *.png .tempimg.ppm

run:
	./mdl spinny.mdl
