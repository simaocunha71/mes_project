### Trabalho prático da UC de Manutenção e Evolução de Software - 2022/2023

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
    
### Otimizações desenvolvidas até ao momento:

  1. Elemento neutro das operações
  
  2. Em estrutura cíclicas converção de condições constantes numéricas para booleans
  
  3. Conversão da estrutura for para while 
  
### Testes

No módulo ```Tests.hs``` tem alguns input exemplos com frases válidas da linguagem. Os inputs de teste podem ser chamados no módulo principal.

Exemplo: ```parse test_10```
