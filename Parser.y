{
module Parser where

import Lexer
}

%name parsePrograma
%tokentype { Token }
%error { parseError }

%token
    'inteiro'      { TokenInteiro }
    'logico'       { TokenLogico }
    'leia'         { TokenLeia }
    'escreva'      { TokenEscrita }
    'se'           { TokenSe }
    'senao'        { TokenSenao }
    'entao'        { TokenEntao }
    'enquanto'     { TokenEnquanto }
    ';'            { TokenPontoVirgula }
    ','            { TokenVirgula }
    '='            { TokenIgual }
    '('            { TokenAbreParenteses }
    ')'            { TokenFechaParenteses }
    '{'            { TokenAbreChaves }
    '}'            { TokenFechaChaves }
    '||'           { TokenOu }
    '&&'           { TokenE }
    '=='           { TokenIgualIgual }
    '!='           { TokenDiferente }
    '<'            { TokenMenor }
    '<='           { TokenMenorIgual }
    '>'            { TokenMaior }
    '>='           { TokenMaiorIgual }
    '+'            { TokenMais }
    '-'            { TokenMenos }
    '*'            { TokenVezes }
    '/'            { TokenDiv }
    '%'            { TokenMod }
    '!'            { TokenNao }
    string         { TokenCadeia $$ }
    integer        { TokenNum $$ }
    id             { TokenId $$ }
    comentario     { TokenComentario $$ }

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
    :                        { [] }
    | Comandos Comando       { $1 ++ [$2] }

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
    : Atribuicao ';'        { CmdAtrib $1 }
    | Leitura ';'           { CmdLeitura $1 }
    | Escrita ';'           { CmdEscrita $1 }
    | Se                     { CmdSe $1}
    | Enquanto               { CmdEnquanto $1 }
    | comentario             { CmdComentario $1 }

Atribuicao :: { Atribuicao }
    : id '=' Expr               { Atrib $1 $3 }

Leitura :: { Leitura }
    : 'leia' '(' ListaIds ')'   { Leitura $3 }

Escrita :: { Escrita }
    : 'escreva' '(' ListaExp ')' { Escrita $3 }

Se :: { Se }
    : 'se' '(' Expr ')' 'entao' '{' Comandos '}' %prec 'senao'    { Se $3 $7 [] }
    | 'se' '(' Expr ')' 'entao' '{' Comandos '}' 'senao' '{' Comandos '}' { Se $3 $7 $11 }

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
    | ECadeia String
    deriving (Show)

parseError :: [Token] -> a
parseError _ = error "Parse error"
}