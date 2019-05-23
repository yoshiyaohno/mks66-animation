default: main
	@echo
	@echo rendering image...
	./mdl face.mdl

main: Main.hs Parser.hs Transform.hs Line.hs Solids.hs Screen.hs DrawMats.hs \
		Lighting.hs Lexer.hs
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
	./Main script
