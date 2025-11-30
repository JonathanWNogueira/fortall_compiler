module Interpreter where

import Lexer
import Parser
import SemanticAnalyzer
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Control.Monad (foldM, when)
import Control.Monad.RWS (MonadState(put))

-- Valores na linguagem
data Valor = VInt Int | VBool Bool | VString String deriving (Eq)

instance Show Valor where
    show (VInt n) = show n
    show (VBool True) = "verdadeiro"
    show (VBool False) = "falso"
    show (VString s) = s

-- Execução (mapeia variáveis para valores)
type Execucao = M.Map String Valor

-- Executa um programa
executarPrograma :: Programa -> IO ()
executarPrograma (Programa decls cmds) = do
    -- Inicializa a execução com as variáveis declaradas
    let exe = foldl inicializarVar M.empty decls
    -- Executa comandos
    _ <- executarComandos exe cmds
    return ()
  where
    inicializarVar exe decl = case decl of
        DeclInteiro ids -> foldl (\e id -> M.insert id (VInt 0) e) exe ids -- Inicializa inteiros com 0
        DeclLogico ids  -> foldl (\e id -> M.insert id (VBool False) e) exe ids -- Inicializa lógicos com False

-- Executa uma lista de comandos
executarComandos :: Execucao -> [Comando] -> IO Execucao
executarComandos = foldM executarComando -- Executa uma lista de comandos, retornando o estado final da execução


-- Executa um comando individual
executarComando :: Execucao -> Comando -> IO Execucao
executarComando exe comando = case comando of
    CmdAtrib atrb -> executarAtribuicao exe atrb
    CmdLeitura leit -> executarLeitura exe leit
    CmdEscrita esc -> executarEscrita exe esc >> return exe
    CmdSe se -> executarSe exe se
    CmdEnquanto enq -> executarEnquanto exe enq
    CmdRepita rep -> executarRepita exe rep
    CmdPara p -> executarPara exe p
    CmdComentario _ -> return exe

-- Atribuição
executarAtribuicao :: Execucao -> Atribuicao -> IO Execucao
executarAtribuicao exe (Atrib id expr) = do
    valor <- avaliarExpr exe expr
    return $ M.insert id valor exe -- Atualiza o valor da variável no estado de execução

-- Leitura
executarLeitura :: Execucao -> Leitura -> IO Execucao
executarLeitura exe (Leitura ids) =
    foldM lerVar exe ids -- Lê valores para cada variável
  where
    lerVar exe id = do
        input <- getLine
        putStrLn ""
        case M.lookup id exe of
            Just (VInt _)  -> case readMaybe input of   -- Verifica se a entrada é um inteiro
                Just n -> return $ M.insert id (VInt n) exe
                Nothing -> do
                    putStrLn "Entrada invalida. Digite um inteiro:"
                    lerVar exe id
            Just (VBool _) -> case input of             -- Verifica se a entrada é "verdadeiro" ou "falso"
                "verdadeiro"  -> return $ M.insert id (VBool True) exe
                "falso" -> return $ M.insert id (VBool False) exe
                _ -> do
                    putStrLn "Entrada invalida. Digite 'verdadeiro' ou 'falso':"
                    lerVar exe id

-- Escrita
executarEscrita :: Execucao -> Escrita -> IO ()
executarEscrita exe (Escrita exprs) = do
    resultados <- mapM (avaliarExpr exe) exprs
    mapM_ imprimirItem resultados
    putStrLn ""
  where
    imprimirItem :: Valor -> IO ()
    imprimirItem (VInt n) = putStr (show n)
    imprimirItem (VBool b) = putStr (show b)
    imprimirItem (VString s) = imprimirComEscapes s
     where
        imprimirComEscapes :: String -> IO ()
        imprimirComEscapes [] = return ()
        imprimirComEscapes ('\\':c:xs) = do
            case c of
                'n' -> putStr "\n"  -- Quebra de linha
                't' -> putStr "\t"  -- Tabulação
                '"' -> putStr "\""  -- Aspas duplas
                '\\' -> putStr "\\" -- Barra invertida literal
            imprimirComEscapes xs
        imprimirComEscapes (x:xs) = do
            putChar x
            imprimirComEscapes xs

-- Comando Se
executarSe :: Execucao -> Se -> IO Execucao
executarSe exe (Se expr cmdThen cmdElse) = do
    cond <- avaliarExpr exe expr
    case cond of
        VBool True  -> executarComandos exe cmdThen
        VBool False -> executarComandos exe cmdElse

-- Comando Enquanto
executarEnquanto :: Execucao -> Enquanto -> IO Execucao
executarEnquanto exe (Enquanto expr cmds) = do
    cond <- avaliarExpr exe expr
    case cond of
        VBool True -> do
            exe' <- executarComandos exe cmds
            executarEnquanto exe' (Enquanto expr cmds)
        VBool False -> return exe

-- Comando Repita ... ate
executarRepita :: Execucao -> Repita -> IO Execucao
executarRepita exe (Repita cmds expr) = do
    exe' <- executarComandos exe cmds  -- Executa o bloco primeiro
    cond <- avaliarExpr exe' expr      -- Avalia a condição com o novo estado
    case cond of
        VBool True -> return exe'                             -- Se verdadeiro, termina
        VBool False -> executarRepita exe' (Repita cmds expr) -- Se falso, repete

executarPara :: Execucao -> Para -> IO Execucao
executarPara exe (Para init expr update cmds) = do
    exeInit <- executarAtribuicao exe init              -- Executa inicialização (apenas uma vez)
    loopPara exeInit expr update cmds                   -- Entra no loop
  where
    loopPara :: Execucao -> Expr -> Atribuicao -> [Comando] -> IO Execucao
    loopPara e cond passo corpo = do
        valCond <- avaliarExpr e cond                           -- Avalia condição
        case valCond of
            VBool True -> do
                e' <- executarComandos e corpo                  -- Executa corpo
                e'' <- executarAtribuicao e' passo              -- Executa incremento/passo
                loopPara e'' cond passo corpo                   -- Repete
            VBool False -> return e                             -- Sai do loop
            _ -> error "Erro de tipo em tempo de execucao no 'para'"

-- Avalia uma expressão
avaliarExpr :: Execucao -> Expr -> IO Valor
avaliarExpr exe expr = case expr of
    EOu e1 e2         -> binarioLogico (||) exe e1 e2
    EE e1 e2          -> binarioLogico (&&) exe e1 e2
    EIgual e1 e2      -> comparacao    (==) exe e1 e2
    EDif e1 e2        -> comparacao    (/=) exe e1 e2
    EMenor e1 e2      -> relacional    (<) exe e1 e2
    EMenorIgual e1 e2 -> relacional    (<=) exe e1 e2
    EMaior e1 e2      -> relacional    (>) exe e1 e2
    EMaiorIgual e1 e2 -> relacional    (>=) exe e1 e2
    EMais e1 e2       -> aritmetica    (+) exe e1 e2
    EMenos e1 e2      -> aritmetica    (-) exe e1 e2
    EVezes e1 e2      -> aritmetica    (*) exe e1 e2
    EDiv e1 e2        -> aritmetica    div exe e1 e2
    EMod e1 e2        -> aritmetica    mod exe e1 e2
    ENao e            -> do
        v <- avaliarExpr exe e
        case v of
            VBool b -> return $ VBool (not b)
            _ -> error "Tipo invalido para operador logico"
    EId id            -> return $ fromJust (M.lookup id exe)  -- Seguro após análise
    ENum n            -> return $ VInt n
    EBool "verdadeiro"           -> return $ VBool True
    EBool "falso"                -> return $ VBool False
    ECadeia s         -> return $ VString s
    
-- Funções auxiliares para operações
binarioLogico :: (Bool -> Bool -> Bool) -> Execucao -> Expr -> Expr -> IO Valor
binarioLogico op exe e1 e2 = do
    v1 <- avaliarExpr exe e1
    v2 <- avaliarExpr exe e2
    case (v1, v2) of
        (VBool a, VBool b) -> return $ VBool (a `op` b)
        _ -> error "Operandos logicos devem ser booleanos"

comparacao :: (Valor -> Valor -> Bool) -> Execucao -> Expr -> Expr -> IO Valor
comparacao op exe e1 e2 = do
    v1 <- avaliarExpr exe e1
    v2 <- avaliarExpr exe e2
    return $ VBool (v1 `op` v2)

relacional :: (Int -> Int -> Bool) -> Execucao -> Expr -> Expr -> IO Valor
relacional op exe e1 e2 = do
    v1 <- avaliarExpr exe e1
    v2 <- avaliarExpr exe e2
    case (v1, v2) of
        (VInt a, VInt b) -> return $ VBool (a `op` b)
        _ -> error "Operandos relacionais devem ser inteiros"

aritmetica :: (Int -> Int -> Int) -> Execucao -> Expr -> Expr -> IO Valor
aritmetica op exe e1 e2 = do
    v1 <- avaliarExpr exe e1
    v2 <- avaliarExpr exe e2
    case (v1, v2) of
        (VInt a, VInt b) -> return $ VInt (a `op` b)
        _ -> error "Operandos aritmeticos devem ser inteiros"