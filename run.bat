alex Lexer.x
happy --ghc Parser.y
ghc --make Main.hs -o compiler
del /q *.o *.hi  Lexer.hs Parser.hs
@REM compiler *.txt