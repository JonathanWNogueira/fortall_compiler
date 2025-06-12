alex Lexer.x
ghc --make Main.hs -o lexer
lexer input.txt
del /q *.o *.hi *.exe Lexer.hs