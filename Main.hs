import Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  src <- if null args then getContents else readFile (head args)
  let tokens = alexScanTokens src
  putStrLn "Tokens:"
  mapM_ print tokens
