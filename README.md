# Parser e Simulador da Linguagem Fortall

Este projeto implementa um **parser completo** (analisador léxico, sintático e semântico) e um **simulador** para uma sublinguagem da linguagem **Fortall**, uma sub-linguagem inspirada em C/Pascal. A ferramenta analisa e executa programas escritos nessa linguagem, reportando erros quando necessário.

## Especificação da Linguagem Fortall

A linguagem Fortall inclui:

- Tipos: `inteiro`, `decimal`, `lógico`, `texto`

- Estruturas de controle: `se-entao-senao`, `enquanto`, `repita ... até`, `para`

- Entrada e saída: leitura de variáveis e escrita de mensagens/valores

- Comentários no estilo C: `/* ... */` (sem aninhamento)

- Cadeias de caracteres com escapes `\n` e `\t`

- Expressões com precedência de operadores igual à linguagem C

## Objetivo do Projeto

Desenvolver um **parser completo** para a linguagem Fortall, incluindo:

- Analisador léxico

- Analisador sintático

- Analisador semântico

- Simulador de execução de código válido

- Tratamento de erros com localização e mensagens claras

## Pré-requisitos

Para compilar e executar este projeto, você precisará:

- **Sistema Operacional**: Linux, macOS ou Windows

- **GHC (Glasgow Haskell Compiler)**: Versão 8.10 ou superior

- **Gerenciador de Pacotes Cabal**

- **Alex e Happy**: Geradores de analisadores léxico e sintático

## Instalação

Siga os passos abaixo para instalar as dependências:

### 1. Instalar o GHC e Cabal

- **Linux**:

```bash

sudo apt-get update

sudo apt-get install ghc cabal-install

```

- **macOS (via Homebrew)**:

```bash

brew install ghc cabal-install

```

- **Windows (via Chocolatey)**:

```bash

choco install haskell-dev

```

### 2. Instalar Alex e Happy

```bash

cabal update

cabal install alex happy

```

### 3. Configurar Variáveis de Ambiente

Adicione o diretório de binários do Cabal ao seu PATH:

```bash

export PATH=$PATH:~/.cabal/bin

```

## Como Executar

1. Compile os arquivos necessários com GHC utilizando o script abaixo:

```bash

alex Lexer.x
happy Parser.y
ghc --make Main.hs -o interpreter

```

2. Execute o parser com um arquivo fonte como entrada:

```bash

interpreter input.txt

```

3. O analisador irá:

- Exibir os tokens e árvore sintática

- Verificar semântica

- Executar o código caso não haja erros

- Informar erros com linha/coluna e mensagem descritiva

## Nuances da Implementação

### Analisador Léxico (Lexer.x)

- Implementado com **Alex**

- Tratamento especial para strings com sequências de escape (`\n`, `\t`, `\\`, `\"`)

- Detecção de erros léxicos:

- Strings não fechadas

- Comentários não fechados

- Caracteres de escape inválidos

- Caracteres não reconhecidos

- Geração de tokens com informações de posição (linha/coluna)

### Analisador Sintático (Parser.y)

- Implementado com **Happy** usando gramática LALR(1)

- Precedência explícita de operadores (igual à linguagem C)

- Tratamento de expressões unárias (negação `!` e negativo `-`)

- Construção de AST (Árvore Sintática Abstrata) tipada

- Recuperação de erros sintáticos com relatório preciso

### Analisador Semântico (SemanticAnalyzer.hs)

- Verificação em duas fases:

1. Declarações (variáveis duplicadas/não declaradas)

2. Comandos e expressões (compatibilidade de tipos)

- Sistema de tipos:

- `Inteiro`: Números inteiros

- `Decimal`: Números reais

- `Logico`: Valores booleanos (`verdadeiro`/`falso`)

- `Texto`: Strings

- Verificações principais:

   -  Atribuições com tipos compatíveis

   - Expressões booleanas em estruturas de controle

   - Operandos válidos para operadores

   - Variáveis declaradas antes do uso

   - Uso de monads (`Either`) para tratamento de erros

### Interpretador (Interpreter.hs)

O interpretador executa programas válidos (após análise semântica) utilizando as seguintes características:

- **Representação de valores**:

   - `VInt Int`: Valores inteiros

   - `VFloat Double`: Valores reais

   - `VBool Bool`: Valores booleanos

   - `VString String`: Cadeias de caracteres (com tratamento de escapes)

   - **Estado de execução**: Mapeamento de variáveis para valores (`Execucao = M.Map String Valor`)

- **Funcionalidades**:

- **Atribuição**: Atualiza o valor de variáveis no estado de execução

- **Leitura**: Lê valores do terminal com tratamento de entrada inválida

- **Escrita**: Imprime valores no terminal, tratando escapes em strings (`\n`, `\t`, `\"`, `\\`)

- **Controle de fluxo**:

   - `se`: Executa blocos condicionais

   - `enquanto`, `repita ... ate`, `para`: Executa loops enquanto a condição for verdadeira

- **Avaliação de expressões**:

   - Suporte completo a operações lógicas, aritméticas, relacionais e comparação

   - Tratamento de erros de tipo em tempo de execução (não deve ocorrer após análise semântica)

### Tratamento de Erros

   - Erros léxicos/sintáticos: Reportados com linha/coluna exatas

   - Erros semânticos: Mensagens descritivas com contexto

   - Tipos incompatíveis

   - Variáveis não declaradas

   - Declarações duplicadas

   - Uso incorreto em estruturas de controle

## Exemplo de Script

Arquivo de entrada (`exemplo.txt`):

```fortall

inteiro x, y;
logico flag;

leia(x, y);

flag = x > y;

se (flag) entao {
   escreva("x eh maior que y");
} 

senao {
   escreva("y eh maior ou igual a x");
}

```

## Estrutura de Arquivos

- `Lexer.x`: Especificação do analisador léxico

- `Parser.y`: Especificação do analisador sintático

- `SemanticAnalyzer.hs`: Implementação do analisador semântico.

- `Interpreter.hs`: Implementação do simulador.
