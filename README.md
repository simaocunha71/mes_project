# mes_project

 Para executar o projeto correr o módulo principal:

```ghci Runner.hs```

Este modulo disponibiliza as seguintes funções:

 - Parse de um input que respeita a linguagem gerando uma ast

     ```parse :: String -> Program```

- Gera frase válida da linguagem apartir de uma ast

     ```unparse :: Program -> String```

-  Parse de um input que respeita a linguagem gerando uma ast já com otimizações aplicadas

    ```parseWithOpt :: String -> Program```

- Aplica à ast as otimizações

    ```applyOpts:: Program -> Program```
    
Otimizações desenvolvidas até ao momento:

  1. Elemento neutro das operações
  
  2. Em estrutura cíclicas converção de condições constantes numéricas para booleans
  
  3. Conversão da estrutura for para while 
