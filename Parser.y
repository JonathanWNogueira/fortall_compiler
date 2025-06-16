{
module Parser where

import Lexer
import Data.List (intercalate)
}

%name parsePrograma
%tokentype { TokenWithPos }
%error { parseError }

%token
    'inteiro'      { TokenWithPos TokenInteiro _ }
    'logico'       { TokenWithPos TokenLogico _ }
    'leia'         { TokenWithPos TokenLeia _ }
    'escreva'      { TokenWithPos TokenEscrita _ }
    'se'           { TokenWithPos TokenSe _ }
    'senao'        { TokenWithPos TokenSenao _ }
    'entao'        { TokenWithPos TokenEntao _ }
    'enquanto'     { TokenWithPos TokenEnquanto _ }
    'verdadeiro'   { TokenWithPos TokenV _ }  
    'falso'        { TokenWithPos TokenF _ }       
    ';'            { TokenWithPos TokenPontoVirgula _ }
    ','            { TokenWithPos TokenVirgula _ }
    '='            { TokenWithPos TokenIgual _ }
    '('            { TokenWithPos TokenAbreParenteses _ }
    ')'            { TokenWithPos TokenFechaParenteses _ }
    '{'            { TokenWithPos TokenAbreChaves _ }
    '}'            { TokenWithPos TokenFechaChaves _ }
    '||'           { TokenWithPos TokenOu _ }
    '&&'           { TokenWithPos TokenE _ }
    '=='           { TokenWithPos TokenIgualIgual _ }
    '!='           { TokenWithPos TokenDiferente _ }
    '<'            { TokenWithPos TokenMenor _ }
    '<='           { TokenWithPos TokenMenorIgual _ }
    '>'            { TokenWithPos TokenMaior _ }
    '>='           { TokenWithPos TokenMaiorIgual _ }
    '+'            { TokenWithPos TokenMais _ }
    '-'            { TokenWithPos TokenMenos _ }
    '*'            { TokenWithPos TokenVezes _ }
    '/'            { TokenWithPos TokenDiv _ }
    '%'            { TokenWithPos TokenMod _ }
    '!'            { TokenWithPos TokenNao _ }
    string         { TokenWithPos (TokenCadeia $$) _ } 
    integer        { TokenWithPos (TokenNum $$) _ }
    id             { TokenWithPos (TokenId $$) _ }
    comentario     { TokenWithPos (TokenComentario $$) _ }

%right '='
%left '||'
%left '&&'
%left '==' '!='
%left '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%right '!' '-'
%right ';'
%right 'senao'

%%

Programa :: { Programa }
    : Decls Comandos            { Programa $1 $2 }

Comandos :: { [Comando] }
    : {- empty -}               { [] }
    | Comandos Comando          { $1 ++ [$2] }

Decls :: { [Decl] }
    : Decl Decls                { $1 : $2 }
    | {- empty -}               { [] }

Decl :: { Decl }
    : 'inteiro' ListaIds ';'    { DeclInteiro $2 }
    | 'logico' ListaIds ';'     { DeclLogico $2 }

ListaIds :: { [String] }
    : id                        { [$1] }
    | id ',' ListaIds           { $1 : $3 }

Comando :: { Comando }
    : Atribuicao ';'            { CmdAtrib $1 }
    | Leitura ';'               { CmdLeitura $1 }
    | Escrita ';'               { CmdEscrita $1 }
    | Se                        { CmdSe $1 }
    | Enquanto                  { CmdEnquanto $1 }
    | comentario                { CmdComentario $1 }

Atribuicao :: { Atribuicao }
    : id '=' Expr               { Atrib $1 $3 }

Leitura :: { Leitura }
    : 'leia' '(' ListaIds ')'   { Leitura $3 }

Escrita :: { Escrita }
    : 'escreva' '(' ListaExp ')' { Escrita $3 }

Se :: { Se }
    : 'se' '(' Expr ')' 'entao' '{' Comandos '}' %prec 'senao'              { Se $3 $7 [] }
    | 'se' '(' Expr ')' 'entao' '{' Comandos '}' 'senao' '{' Comandos '}'   { Se $3 $7 $11 }

Enquanto :: { Enquanto }
    : 'enquanto' '(' Expr ')' '{' Comandos '}' { Enquanto $3 $6 }

ListaExp :: { [Expr] }
    : Expr                      { [$1] }
    | Expr ',' ListaExp         { $1 : $3 }

Expr :: { Expr }
    : Expr '||' Expr1           { EOu $1 $3 }
    | Expr1                     { $1 }

Expr1 :: { Expr }
    : Expr1 '&&' Expr2          { EE $1 $3 }
    | Expr2                     { $1 }

Expr2 :: { Expr }
    : Expr2 '==' Expr3          { EIgual $1 $3 }
    | Expr2 '!=' Expr3          { EDif $1 $3 }
    | Expr3                     { $1 }

Expr3 :: { Expr }
    : Expr3 '<' Expr4           { EMenor $1 $3 }
    | Expr3 '<=' Expr4          { EMenorIgual $1 $3 }
    | Expr3 '>' Expr4           { EMaior $1 $3 }
    | Expr3 '>=' Expr4          { EMaiorIgual $1 $3 }
    | Expr4                     { $1 }

Expr4 :: { Expr }
    : Expr4 '+' Expr5           { EMais $1 $3 }
    | Expr4 '-' Expr5           { EMenos $1 $3 }
    | Expr5                     { $1 }

Expr5 :: { Expr }
    : Expr5 '*' Expr6           { EVezes $1 $3 }
    | Expr5 '/' Expr6           { EDiv $1 $3 }
    | Expr5 '%' Expr6           { EMod $1 $3 }
    | Expr6                     { $1 }

Expr6 :: { Expr }
    : '!' Expr6                 { ENao $2 }
    | '-' Expr6                 { EMenos (ENum 0) $2 }
    | Expr7                     { $1 }

Expr7 :: { Expr }
    : '(' Expr ')'              { $2 }
    | id                        { EId $1 }
    | integer                   { ENum $1 }
    | string                    { ECadeia $1 }
    | 'verdadeiro'              { EBool "verdadeiro" }
    | 'falso'                   { EBool "falso" }

{
data Programa = Programa [Decl] [Comando] deriving (Show)

data Decl
    = DeclInteiro [String]
    | DeclLogico [String]
    deriving (Show)

data Comando
    = CmdAtrib Atribuicao
    | CmdLeitura Leitura
    | CmdEscrita Escrita
    | CmdSe Se
    | CmdEnquanto Enquanto
    | CmdComentario String
    deriving (Show)

data Atribuicao = Atrib String Expr deriving (Show)
data Leitura = Leitura [String] deriving (Show)
data Escrita = Escrita [Expr] deriving (Show)
data Se = Se Expr [Comando] [Comando] deriving (Show)
data Enquanto = Enquanto Expr [Comando] deriving (Show)

data Expr
    = EOu Expr Expr
    | EE Expr Expr
    | EIgual Expr Expr
    | EDif Expr Expr
    | EMenor Expr Expr
    | EMenorIgual Expr Expr
    | EMaior Expr Expr
    | EMaiorIgual Expr Expr
    | EMais Expr Expr
    | EMenos Expr Expr
    | EVezes Expr Expr
    | EDiv Expr Expr
    | EMod Expr Expr
    | ENao Expr
    | EId String
    | ENum Int
    | EBool String
    | ECadeia String
    deriving (Show)

-- Pretty-printing functions
prettyPrint :: Programa -> String
prettyPrint (Programa decls cmds) =
  "Declaracoes:\n" ++ intercalate "\n" (map prettyDecl decls) ++
  "\n\nComandos:\n" ++ intercalate "\n" (map prettyComando cmds)

prettyDecl :: Decl -> String
prettyDecl (DeclInteiro ids) = "Inteiro: " ++ intercalate ", " ids
prettyDecl (DeclLogico ids)  = "Logico: " ++ intercalate ", " ids

prettyComando :: Comando -> String
prettyComando (CmdAtrib (Atrib id expr)) = id ++ " = " ++ prettyExpr expr
prettyComando (CmdLeitura (Leitura ids)) = "leia(" ++ intercalate ", " ids ++ ")"
prettyComando (CmdEscrita (Escrita exprs)) = "escreva(" ++ intercalate ", " (map prettyExpr exprs) ++ ")"
prettyComando (CmdSe (Se expr thenCmds elseCmds)) =
  "se(" ++ prettyExpr expr ++ ") entao {\n" ++
  intercalate "\n" (map (indent 2 . prettyComando) thenCmds) ++
  "\n}" ++ (if not (null elseCmds)
            then " senao {\n" ++ intercalate "\n" (map (indent 2 . prettyComando) elseCmds) ++ "\n}"
            else "")
prettyComando (CmdEnquanto (Enquanto expr cmds)) =
  "enquanto(" ++ prettyExpr expr ++ ") {\n" ++
  intercalate "\n" (map (indent 2 . prettyComando) cmds) ++ "\n}"
prettyComando (CmdComentario coment) = "/*" ++ coment ++ "*/"

prettyExpr :: Expr -> String
prettyExpr (EOu e1 e2)      = prettyExpr e1 ++ " || " ++ prettyExpr e2
prettyExpr (EE e1 e2)       = prettyExpr e1 ++ " && " ++ prettyExpr e2
prettyExpr (EIgual e1 e2)   = prettyExpr e1 ++ " == " ++ prettyExpr e2
prettyExpr (EDif e1 e2)     = prettyExpr e1 ++ " != " ++ prettyExpr e2
prettyExpr (EMenor e1 e2)   = prettyExpr e1 ++ " < " ++ prettyExpr e2
prettyExpr (EMenorIgual e1 e2) = prettyExpr e1 ++ " <= " ++ prettyExpr e2
prettyExpr (EMaior e1 e2)   = prettyExpr e1 ++ " > " ++ prettyExpr e2
prettyExpr (EMaiorIgual e1 e2) = prettyExpr e1 ++ " >= " ++ prettyExpr e2
prettyExpr (EMais e1 e2)    = prettyExpr e1 ++ " + " ++ prettyExpr e2
prettyExpr (EMenos e1 e2)   = prettyExpr e1 ++ " - " ++ prettyExpr e2
prettyExpr (EVezes e1 e2)   = prettyExpr e1 ++ " * " ++ prettyExpr e2
prettyExpr (EDiv e1 e2)     = prettyExpr e1 ++ " / " ++ prettyExpr e2
prettyExpr (EMod e1 e2)     = prettyExpr e1 ++ " % " ++ prettyExpr e2
prettyExpr (ENao e)         = "!" ++ prettyExpr e
prettyExpr (EId id)         = id
prettyExpr (ENum n)         = show n
prettyExpr (EBool b)        = if b == "verdadeiro" then "verdadeiro" else "falso"
prettyExpr (ECadeia s)      = "\"" ++ s ++ "\""

indent :: Int -> String -> String
indent n s = replicate (n*2) ' ' ++ s

-- Funções para extrair linha e coluna
alexLine :: AlexPosn -> Int
alexLine (AlexPn _ line _) = line

alexColumn :: AlexPosn -> Int
alexColumn (AlexPn _ _ col) = col

parseError :: [TokenWithPos] -> a
parseError [] = error "ERRO SINTÁTICO (posição desconhecida)"
parseError (tok@(TokenWithPos _ pos) : _) = 
  error $ "ERRO SINTÁTICO (linha " ++ show (alexLine pos) 
          ++ ", coluna " ++ show (alexColumn pos) ++ ")\n"
          ++ "Token inesperado: " ++ showToken tok

  where
    showToken (TokenWithPos t _) = case t of
      TokenInteiro -> "'inteiro'"
      TokenLogico -> "'logico'"
      TokenLeia -> "'leia'"
      TokenEscrita -> "'escreva'"
      TokenSe -> "'se'"
      TokenSenao -> "'senao'"
      TokenEntao -> "'entao'"
      TokenEnquanto -> "'enquanto'"
      TokenPontoVirgula -> "';'"
      TokenVirgula -> "','"
      TokenIgual -> "'='"
      TokenAbreParenteses -> "'('"
      TokenFechaParenteses -> "')'"
      TokenAbreChaves -> "'{'"
      TokenFechaChaves -> "'}'"
      TokenOu -> "'||'"
      TokenE -> "'&&'"
      TokenIgualIgual -> "'=='"
      TokenDiferente -> "'!='"
      TokenMenor -> "'<'"
      TokenMenorIgual -> "'<='"
      TokenMaior -> "'>'"
      TokenMaiorIgual -> "'>='"
      TokenMais -> "'+'"
      TokenMenos -> "'-'"
      TokenVezes -> "'*'"
      TokenDiv -> "'/'"
      TokenMod -> "'%'"
      TokenNao -> "'!'"
      TokenComentario _ -> "comentário"
      TokenId s -> "identificador '" ++ s ++ "'"
      TokenNum n -> "número " ++ show n
      TokenV -> "bool"
      TokenF -> "bool"
      TokenCadeia s -> "string \"" ++ s ++ "\""
      TokenErro msg _ -> "erro léxico: " ++ msg
}