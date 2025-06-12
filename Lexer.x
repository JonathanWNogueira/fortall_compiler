{
module Lexer where
import Data.Char (isAlpha, isDigit, isAlphaNum)
}

%wrapper "basic"

$digit = 0-9
$letter = [a-zA-Z_]
$alphaNum = [$letter $digit]

tokens :-

<0> $white+ ;  -- Ignora espaços em branco

-- Palavras-chave
<0> "inteiro"       { \s -> TokenInteiro }
<0> "logico"        { \s -> TokenLogico }
<0> "leia"          { \s -> TokenLeia }
<0> "escreva"       { \s -> TokenEscrita }
<0> "se"            { \s -> TokenSe }
<0> "senao"         { \s -> TokenSenao }
<0> "entao"         { \s -> TokenEntao }
<0> "enquanto"      { \s -> TokenEnquanto }

-- Operadores e pontuação
<0> ";"             { \s -> TokenPontoVirgula }
<0> ","             { \s -> TokenVirgula }
<0> "="             { \s -> TokenIgual }
<0> "("             { \s -> TokenAbreParenteses }
<0> ")"             { \s -> TokenFechaParenteses }
<0> "||"            { \s -> TokenOu }
<0> "&&"            { \s -> TokenE }
<0> "=="            { \s -> TokenIgualIgual }
<0> "!="            { \s -> TokenDiferente }
<0> "<"             { \s -> TokenMenor }
<0> "<="            { \s -> TokenMenorIgual }
<0> ">"             { \s -> TokenMaior }
<0> ">="            { \s -> TokenMaiorIgual }
<0> "+"             { \s -> TokenMais }
<0> "-"             { \s -> TokenMenos }
<0> "*"             { \s -> TokenVezes }
<0> "/"             { \s -> TokenDiv }
<0> "%"             { \s -> TokenMod }
<0> "!"             { \s -> TokenNao }

-- Identificadores
<0> $letter $alphaNum* { \s -> TokenId s }

-- Números
<0> $digit+         { \s -> TokenNum (read s) }

-- Comentários delimitados por /* */
<0> "/*" { \s -> TokenComentarioInicio }
<0> "*/" { \s -> TokenComentarioFim }  -- Fim do comentário, o conteúdo será tratado separadamente

-- Strings
<0> \" ([^\"\\] | \\[\"nrt\\])* \" { \s -> TokenCadeia (init (tail s)) }

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
  | TokenComentarioInicio 
  | TokenComentarioFim
  | TokenId String
  | TokenNum Int
  | TokenCadeia String
  deriving (Show, Eq)
}