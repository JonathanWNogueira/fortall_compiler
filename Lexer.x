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
<0> "inteiro"       { \p s -> TokenWithPos TokenInteiro p }
<0> "logico"        { \p s -> TokenWithPos TokenLogico p }
<0> "leia"          { \p s -> TokenWithPos TokenLeia p }
<0> "escreva"       { \p s -> TokenWithPos TokenEscrita p }
<0> "se"            { \p s -> TokenWithPos TokenSe p }
<0> "senao"         { \p s -> TokenWithPos TokenSenao p }
<0> "entao"         { \p s -> TokenWithPos TokenEntao p }
<0> "enquanto"      { \p s -> TokenWithPos TokenEnquanto p }

-- Operadores e pontuação
<0> ";"             { \p s -> TokenWithPos TokenPontoVirgula p }
<0> ","             { \p s -> TokenWithPos TokenVirgula p }
<0> "="             { \p s -> TokenWithPos TokenIgual p }
<0> "("             { \p s -> TokenWithPos TokenAbreParenteses p }
<0> ")"             { \p s -> TokenWithPos TokenFechaParenteses p }
<0> "{"             { \p s -> TokenWithPos TokenAbreChaves p }
<0> "}"             { \p s -> TokenWithPos TokenFechaChaves p }
<0> "||"            { \p s -> TokenWithPos TokenOu p }
<0> "&&"            { \p s -> TokenWithPos TokenE p }
<0> "=="            { \p s -> TokenWithPos TokenIgualIgual p }
<0> "!="            { \p s -> TokenWithPos TokenDiferente p }
<0> "<"             { \p s -> TokenWithPos TokenMenor p }
<0> "<="            { \p s -> TokenWithPos TokenMenorIgual p }
<0> ">"             { \p s -> TokenWithPos TokenMaior p }
<0> ">="            { \p s -> TokenWithPos TokenMaiorIgual p }
<0> "+"             { \p s -> TokenWithPos TokenMais p }
<0> "-"             { \p s -> TokenWithPos TokenMenos p }
<0> "*"             { \p s -> TokenWithPos TokenVezes p }
<0> "/"             { \p s -> TokenWithPos TokenDiv p }
<0> "%"             { \p s -> TokenWithPos TokenMod p }
<0> "!"             { \p s -> TokenWithPos TokenNao p }

-- Identificadores
<0> $letter $alphaNum* { \p s -> TokenWithPos (TokenId s) p }

-- Números
<0> $digit+         { \p s -> TokenWithPos (TokenNum (read s)) p }

-- Comentários delimitados por /* */
<0> "/*" ( [^] | [\*] )* "*/" { \p s -> TokenWithPos (TokenComentario (drop 2 (take (length s - 2) s))) p }

-- Strings
<0> \" ( [^\"\\] | \\\\ | \\\" | \\n | \\t )* \" { \p s -> TokenWithPos (TokenCadeia (init (tail s))) p }

-- Erros léxicos
<0> \\ [^\\nrt\\]                                     { \p s -> TokenWithPos (TokenErro ("Escape invalido: " ++ drop 1 s) p) p }
<0> \" ( [^\"\\] | \\\\ | \\\" | \\n | \\t )*         { \p s -> TokenWithPos (TokenErro "String nao fechada" p) p }
<0> "/*" ( [^] )* \**                                 { \p s -> TokenWithPos (TokenErro "Comentario nao fechado" p) p }
<0> .                                                 { \p s -> TokenWithPos (TokenErro ("Caractere invalido: " ++ s) p) p }

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

data TokenWithPos = TokenWithPos Token AlexPosn deriving (Show)

alexScanTokensWithPos :: String -> [TokenWithPos]
alexScanTokensWithPos = alexScanTokens

stripTokenPos :: TokenWithPos -> Token
stripTokenPos (TokenWithPos token _) = token
}