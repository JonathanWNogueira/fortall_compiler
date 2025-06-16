{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use tuple-section" #-}
module SemanticAnalyzer where

import Lexer
import Parser
import qualified Data.Map as M
import Data.List (nub)
import GHC.ResponseFile (escapeArgs)


-- Tipos da linguagem
data Tipo = Inteiro | Logico deriving (Eq, Show)

-- Contexto (mapeia variáveis para tipos)
type Contexto = M.Map String Tipo

-- Erros semânticos
type Erro = String

-- Resultado da análise semântica
type Resultado = Either Erro ()

-- Verificação do programa principal
verificaPrograma :: Programa -> Resultado
verificaPrograma (Programa decls cmds) = do
    contexto <- verificaDecls decls -- Verifica as declarações
    verificaComandos contexto cmds  -- Verifica os comandos

-- Verifica declarações e constrói o contexto
verificaDecls :: [Decl] -> Either Erro Contexto
verificaDecls decls = 
    let (vars, dups) = foldl extraiVars ([], []) decls
    in if null dups
        then Right (M.fromList vars) -- Se não há duplicatas, retorna o contexto
        else Left ("Variavel declarada multiplas vezes: " ++ unwords (nub dups)) -- Há duplicatas
  where
    extraiVars (vars, dups) decl = case decl of                -- Declaração de variável
        DeclInteiro ids -> processaIds ids Inteiro vars dups   -- Processa IDs de inteiros
        DeclLogico ids  -> processaIds ids Logico vars dups    -- Processa IDs lógicos
    
    processaIds ids tipo vars dups =                                -- Processa IDs e tipos
        let novasDups = filter (`elem` map fst vars) ids            -- Verifica duplicatas
        in (vars ++ map (\id -> (id, tipo)) ids, dups ++ novasDups) -- Adiciona novos IDs ao contexto e duplicatas

-- Verifica lista de comandos
verificaComandos :: Contexto -> [Comando] -> Resultado
verificaComandos _ [] = Right () -- não há comandos a verificar
verificaComandos contexto (c:cs) = do -- Verifica o primeiro comando
    verificaComando contexto c        -- Se não houver erro, continua com os demais
    verificaComandos contexto cs      -- Verifica os demais comandos

-- Verifica um comando individual
verificaComando :: Contexto -> Comando -> Resultado
verificaComando contexto comando = case comando of      -- Verifica o tipo de comando
    CmdAtrib atrb -> verificaAtribuicao contexto atrb   -- Verifica atribuição
    CmdLeitura leit -> verificaLeitura contexto leit    -- Verifica leitura
    CmdEscrita esc -> verificaEscrita contexto esc      -- Verifica escrita
    CmdSe se -> verificaSe contexto se                  -- Verifica se
    CmdEnquanto enq -> verificaEnquanto contexto enq    -- Verifica enquanto
    CmdComentario _ -> Right ()                         -- Comentários são ignorados

-- Verifica atribuição
verificaAtribuicao :: Contexto -> Atribuicao -> Resultado
verificaAtribuicao contexto (Atrib id expr) = do        -- Verifica se a variável está no contexto
    tipoExpr <- verificaExpr contexto expr              -- Verifica o tipo da expressão
    case M.lookup id contexto of                        -- Procura o tipo da variável no contexto
        Nothing -> Left ("Variavel nao declarada: " ++ id)
        Just tipoVar ->
            if tipoVar == tipoExpr -- Verifica se o tipo da variável é compatível com o tipo da expressão 
                then Right ()
                else Left ("Atribuicao de tipo incorreto em '" ++ id ++ "'. Esperado: " ++ show tipoVar ++ ", Obtido: " ++ show tipoExpr)

-- Verifica comando leia
verificaLeitura :: Contexto -> Leitura -> Resultado
verificaLeitura contexto (Leitura ids) = -- Verifica se todas as variáveis estão declaradas
    mapM_ verificaId ids 
  where
    verificaId id = case M.lookup id contexto of
        Nothing -> Left ("Variavel nao declarada: " ++ id)
        Just Inteiro -> Right ()  -- Só permite inteiros
        Just Logico -> Left ("Tipo logico nao suportado em leitura: " ++ id)

-- Verifica comando escreva
verificaEscrita :: Contexto -> Escrita -> Resultado
verificaEscrita contexto (Escrita exprs) = 
    mapM_ (verificaExpr contexto) exprs >> Right () -- Verifica cada expressão e ignora o resultado

-- Verifica comando se
verificaSe :: Contexto -> Se -> Resultado
verificaSe contexto (Se expr cmdThen cmdElse) = do  
    tipoExpr <- verificaExpr contexto expr          
    if tipoExpr == Logico
        then do
            verificaComandos contexto cmdThen
            verificaComandos contexto cmdElse
        else Left ("Expressao do 'se' deve ser logica") 

-- Verifica comando enquanto
verificaEnquanto :: Contexto -> Enquanto -> Resultado
verificaEnquanto contexto (Enquanto expr cmds) = do
    tipoExpr <- verificaExpr contexto expr
    if tipoExpr == Logico
        then verificaComandos contexto cmds
        else Left ("Expressao do 'enquanto' deve ser logica")  

-- Verifica expressões e infere tipos
verificaExpr :: Contexto -> Expr -> Either Erro Tipo
verificaExpr contexto expr = case expr of
    EOu e1 e2         -> verificaBinarioLogico contexto e1 e2 -- Verifica operador lógico OU
    EE e1 e2          -> verificaBinarioLogico contexto e1 e2 -- Verifica operador lógico E
    EIgual e1 e2      -> verificaComparacao contexto e1 e2    -- Verifica igualdade
    EDif e1 e2        -> verificaComparacao contexto e1 e2    -- Verifica diferença
    EMenor e1 e2      -> verificaRelacional contexto e1 e2    -- Verifica menor
    EMenorIgual e1 e2 -> verificaRelacional contexto e1 e2    -- Verifica menor ou igual
    EMaior e1 e2      -> verificaRelacional contexto e1 e2    -- Verifica maior
    EMaiorIgual e1 e2 -> verificaRelacional contexto e1 e2    -- Verifica maior ou igual
    EMais e1 e2       -> verificaAritmetico contexto e1 e2    -- Verifica adição
    EMenos e1 e2      -> verificaAritmetico contexto e1 e2    -- Verifica subtração
    EVezes e1 e2      -> verificaAritmetico contexto e1 e2    -- Verifica multiplicação
    EDiv e1 e2        -> verificaAritmetico contexto e1 e2    -- Verifica divisão
    EMod e1 e2        -> verificaAritmetico contexto e1 e2    -- Verifica módulo
    ENao e            -> verificaNao contexto e               -- Verifica negação
    EId id            -> case M.lookup id contexto of         -- Verifica identificador
        Nothing -> Left ("Variavel nao declarada: " ++ id)
        Just t -> Right t                                     -- Retorna o tipo da variável
    ENum _            -> Right Inteiro                        -- Números são considerados inteiros
    EBool _           -> Right Logico                         -- Booleanos são considerados lógicos
    ECadeia _         -> Right Inteiro                        -- Strings são consideradas inteiros

-- Funções auxiliares para verificação de expressões
verificaBinarioLogico :: Contexto -> Expr -> Expr -> Either Erro Tipo
verificaBinarioLogico contexto e1 e2 = do                     -- Verifica os tipos dos operandos
    t1 <- verificaExpr contexto e1
    t2 <- verificaExpr contexto e2
    if t1 == Logico && t2 == Logico                           -- Ambos devem ser lógicos                           
        then Right Logico 
        else Left "Operadores logicos requerem operandos booleanos"

verificaComparacao :: Contexto -> Expr -> Expr -> Either Erro Tipo
verificaComparacao contexto e1 e2 = do                        -- Verifica os tipos dos operandos
    t1 <- verificaExpr contexto e1
    t2 <- verificaExpr contexto e2
    if t1 == t2                                               -- Ambos devem ser do mesmo tipo
        then Right Logico
        else Left "Comparacao requer operandos do mesmo tipo"

verificaRelacional :: Contexto -> Expr -> Expr -> Either Erro Tipo
verificaRelacional contexto e1 e2 = do                        -- Verifica os tipos dos operandos
    t1 <- verificaExpr contexto e1 
    t2 <- verificaExpr contexto e2
    if t1 == Inteiro && t2 == Inteiro                         -- Ambos devem ser inteiros
        then Right Logico
        else Left "Operadores relacionais requerem operandos inteiros"

verificaAritmetico :: Contexto -> Expr -> Expr -> Either Erro Tipo
verificaAritmetico contexto e1 e2 = do                        -- Verifica os tipos dos operandos
    t1 <- verificaExpr contexto e1
    t2 <- verificaExpr contexto e2
    if t1 == Inteiro && t2 == Inteiro                         -- Ambos devem ser inteiros
        then Right Inteiro
        else Left "Operadores aritméticos requerem operandos inteiros"

verificaNao :: Contexto -> Expr -> Either Erro Tipo
verificaNao contexto e = do                                   -- Verifica o tipo do operando da negação
    t <- verificaExpr contexto e
    if t == Logico                                            -- O operador de negação só pode ser aplicado a booleanos
        then Right Logico
        else Left "Operador '!' requer operando booleano"