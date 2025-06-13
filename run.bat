alex Lexer.x
happy --ghc Parser.y
ghc --make Main.hs -o parser
parser input.txt
del /q *.o *.hi *.exe Lexer.hs Parser.hs