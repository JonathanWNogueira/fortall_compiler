{
module Lexer where
import Data.Char (isAlpha, isDigit, isAlphaNum)
}

%wrapper "posn"

$digit = 0-9
$letter = [a-zA-Z_]
$alphaNum = [$letter $digit]

tokens :-

<0> $white+ ;  -- Ignora espaços em branco

-- Palavras-chave
<0> "inteiro"       { \_ _ -> TokenInteiro }
<0> "logico"        { \_ _ -> TokenLogico }
<0> "leia"          { \_ _ -> TokenLeia }
<0> "escreva"       { \_ _ -> TokenEscrita }
<0> "se"            { \_ _ -> TokenSe }
<0> "senao"         { \_ _ -> TokenSenao }
<0> "entao"         { \_ _ -> TokenEntao }
<0> "enquanto"      { \_ _ -> TokenEnquanto }

-- Operadores e pontuação
<0> ";"             { \_ _ -> TokenPontoVirgula }
<0> ","             { \_ _ -> TokenVirgula }
<0> "="             { \_ _ -> TokenIgual }
<0> "("             { \_ _ -> TokenAbreParenteses }
<0> ")"             { \_ _ -> TokenFechaParenteses }
<0> "{"             { \_ _ -> TokenAbreChaves }
<0> "}"             { \_ _ -> TokenFechaChaves }
<0> "||"            { \_ _ -> TokenOu }
<0> "&&"            { \_ _ -> TokenE }
<0> "=="            { \_ _ -> TokenIgualIgual }
<0> "!="            { \_ _ -> TokenDiferente }
<0> "<"             { \_ _ -> TokenMenor }
<0> "<="            { \_ _ -> TokenMenorIgual }
<0> ">"             { \_ _ -> TokenMaior }
<0> ">="            { \_ _ -> TokenMaiorIgual }
<0> "+"             { \_ _ -> TokenMais }
<0> "-"             { \_ _ -> TokenMenos }
<0> "*"             { \_ _ -> TokenVezes }
<0> "/"             { \_ _ -> TokenDiv }
<0> "%"             { \_ _ -> TokenMod }
<0> "!"             { \_ _ -> TokenNao }

-- Identificadores
<0> $letter $alphaNum* { \_ s -> TokenId s }

-- Números
<0> $digit+         { \_ s -> TokenNum (read s) }

-- Comentários delimitados por /* */
<0> "/*" ( [^] | \*)* "*/" { \_ s -> TokenComentario (drop 2 (take (length s - 2) s)) }

-- Strings
<0> \" ([^\"\\] | \\\\ | \\\" | \\n | \\t)* \" { \_ s -> TokenCadeia (init (tail s)) }

-- Erros léxicos
<0> \\ [^\\nrt\\]                                     { \p s -> TokenErro ("Escape invalido: " ++ drop 1 s) p }
<0> \" ([^\"\\] | \\\\ | \\\" | \\n | \\t)*           { \p s -> TokenErro "String nao fechada" p }
<0> "/*" ([^])* \**                                   { \p s -> TokenErro "Comentario nao fechado" p }
<0> .                                                 { \p s -> TokenErro ("Caractere invalido: " ++ s) p }

{
data Token
  = TokenInteiro
  | TokenLogico
  | TokenLeia
  | TokenEscrita
  | TokenSe
  | TokenSenao
  | TokenEntao
  | TokenEnquanto
  | TokenPontoVirgula
  | TokenVirgula
  | TokenIgual
  | TokenAbreParenteses
  | TokenFechaParenteses
  | TokenAbreChaves
  | TokenFechaChaves
  | TokenOu
  | TokenE
  | TokenIgualIgual
  | TokenDiferente
  | TokenMenor
  | TokenMenorIgual
  | TokenMaior
  | TokenMaiorIgual
  | TokenMais
  | TokenMenos
  | TokenVezes
  | TokenDiv
  | TokenMod
  | TokenNao
  | TokenComentario String
  | TokenId String
  | TokenNum Int
  | TokenCadeia String
  | TokenErro String AlexPosn
  deriving (Show, Eq)
}