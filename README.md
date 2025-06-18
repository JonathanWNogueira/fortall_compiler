# Parser e Simulador da Linguagem Fortall

Este projeto implementa um **parser completo** (analisador léxico, sintático e semântico) e um **simulador** para a linguagem **Fortall**, uma sub-linguagem inspirada em C/Pascal, desenvolvida como tema de TCC na UFSM. A ferramenta analisa e executa programas escritos nessa linguagem, reportando erros quando necessário.

## Especificação da Linguagem Fortall

A linguagem Fortall inclui:

- Tipos: `inteiro`, `lógico`
- Estruturas de controle: `enquanto`, `se-entao-senao`
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

## Como Executar

1. Compile os arquivos necessários com GHC utilizando `run.bat`.
2. Execute o parser com um arquivo fonte como entrada.
3. O analisador irá:
   - Exibir os tokens e árvore sintática
   - Verificar semântica
   - Executar o código caso não haja erros
   - Informar erros com linha/coluna e mensagem descritiva
