import Lexer
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  src <- readFile (head args)
  let tokens = alexScanTokens src
  
  putStrLn "=== Tokens ==="
  mapM_ print tokens

  putStrLn "\n=== Análise Sintática ==="
  let ast = parsePrograma tokens
  print ast
