import Lexer
import Parser
import System.Environment (getArgs)
import Control.Exception (catch, ErrorCall(..), evaluate)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Parsec (tokens)

main :: IO ()
main = do
  args <- getArgs
  src <- readFile (head args)
  
  -- Análise léxica
  let tokens = alexScanTokensWithPos src
      lexicalErrors = [t | t@(TokenWithPos (TokenErro _ _) _) <- tokens]
  
  if not (null lexicalErrors)
    then do
      hPutStrLn stderr "=== ERROS LÉXICOS ==="
      mapM_ printLexicalError lexicalErrors
      exitFailure
    else do
      putStrLn "=== Tokens ==="
      mapM_ (print . stripTokenPos) tokens
      
      -- Análise sintática
      catch (do
          programa <- evaluate (parsePrograma tokens)
          putStrLn "=== AST do Programa ==="
          print programa
        )
        handleParseError

printLexicalError :: TokenWithPos -> IO ()
printLexicalError (TokenWithPos (TokenErro msg (AlexPn _ line col)) _) = 
  hPutStrLn stderr $ "ERRO LÉXICO (linha " ++ show line ++ ", coluna " ++ show col ++ "): " ++ msg
printLexicalError _ = return ()

handleParseError :: ErrorCall -> IO ()
handleParseError (ErrorCall msg) = do
  hPutStrLn stderr "=== ERRO SINTÁTICO ==="
  hPutStrLn stderr msg
  exitFailure