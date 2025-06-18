alex Lexer.x
happy --ghc Parser.y
ghc --make Main.hs -o interpreter
del /q *.o *.hi  Lexer.hs Parser.hs
@REM interpreter *.txt