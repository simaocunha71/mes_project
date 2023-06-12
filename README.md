### Trabalho prático da UC de Manutenção e Evolução de Software - 2022/2023

Relatório: [aqui](https://github.com/simaocunha71/mes_project/blob/main/relatorio.pdf)

#### Grupo 10

Simão Cunha (a93262)

Gonçalo Pereira (a93168)

Luís Silva (pg50564)

### Execução

 Para executar o projeto correr o módulo principal:

```ghci Runner.hs```

Este modulo disponibiliza as seguintes funções:

 - Parse de um input gerando uma ast

     ```parse :: String -> Program```

- Gera frase válida da linguagem apartir de uma ast

     ```unparse :: Program -> String```

-  Parse de um input gerando uma ast já com otimizações aplicadas

    ```parseWithOpt :: String -> Program```

- Aplica à ast as otimizações

    ```applyOpts:: Program -> Program```
    
- Parse de um input com smell refactoring
 
  ```parseWithRefactor :: String -> Program```
  
- Aplica à ast smell refactoring

  ```applyRefactor :: Program -> Program```
  
 - Parse de um input indicando o número de bugs
 
   ```bugCount :: String -> Int```   
   
 - Parse de um input retornando para cada programa as funções declaradas e em cada função as variáveis que foram
declaradas e usadas

   ```gatherProgramData :: String -> [(String,[String],[String])]```
 
 - Gerador automático de casos de teste
 
   ```autoTestCaseGen :: Int -> Int -> Int -> IO Program```
   
  - Gerador de mutantes

    ```programMutationsGen :: Program -> Gen Program```
    
  - Teste de propriedades
  
    ```prop_PrintParse :: String -> Bool```
    
    ```prop_OptInnermostTP :: String -> Bool```
    
    ```prop_SmellCommutativeOpt :: String -> Bool```
   
### Otimizações desenvolvidas até ao momento:

  1. Elemento neutro das operações
  
  2. Em estrutura cíclicas converção de condições constantes numéricas para booleans
  
  3. Conversão da estrutura for para while 
  
### Testes

No módulo ```Tests.hs``` tem alguns input exemplos com frases válidas da linguagem. Os inputs de teste podem ser chamados no módulo principal.

Exemplo: ```parse test_10```
