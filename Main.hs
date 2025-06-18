import Lexer
import Parser
import SemanticAnalyzer
import Simulator
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
      mapM_ printLexicalError lexicalErrors
      exitFailure
    else do
      putStrLn "\n=== Tokens ==="
      mapM_ (print . stripTokenPos) tokens
      
      -- Análise sintática
      catch (do
          programa <- evaluate (parsePrograma tokens)
          putStrLn "\n=== AST do Programa ==="
          -- putStrLn $ prettyPrint programa
          print programa

          -- Análise semântica
          case verificaPrograma programa of
            Left err -> do
                hPutStrLn stderr "\n=== ERROS SEMANTICOS ==="
                hPutStrLn stderr err
                exitFailure
            Right _ -> do
                putStrLn "\n=== Analise Semantica CHECK ==="
                putStrLn "\n=== Execucao ==="
                catch (executarPrograma programa) 
                      (\(e :: ErrorCall) -> do
                          hPutStrLn stderr $ "Erro durante execucao: " ++ show e
                          exitFailure)
        )
        handleParseError

printLexicalError :: TokenWithPos -> IO ()
printLexicalError (TokenWithPos (TokenErro msg (AlexPn _ line col)) _) = 
  hPutStrLn stderr $ "ERRO LEXICO (linha " ++ show line ++ ", coluna " ++ show col ++ "): " ++ msg
printLexicalError _ = return ()

handleParseError :: ErrorCall -> IO ()
handleParseError (ErrorCall msg) = do
  hPutStrLn stderr msg
  exitFailure