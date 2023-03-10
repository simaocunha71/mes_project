module Exp where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast

-- "3 + 4 * 2"
e :: Exp
e = Add (Const 3) (Mul (Const 4) (Const 2))

-- Gramática //TODO adicionar aqui os precedence levels, o pFactor nao apanha numeros com digitos > 1, pode haver erros no enclosedBy
-- Exp    -> Termo '+' Exp
--        |  Termo
-- Termo  -> Factor '*' Termo
--        |  Factor
-- Factor -> int
--        |  var
--        |  '('  Exp ')'

pExp :: Parser Exp
pExp = id <$> pExp1

pExp1 :: Parser Exp
pExp1 = id <$> pExp2

pExp2 :: Parser Exp
pExp2 =  f <$> pExp3 <*> token' "||" <*> pExp2
     <|> id <$> pExp3
        where f a _ c = Or a c
        
pExp3 :: Parser Exp
pExp3 = f <$> pExp4 <*> token' "&&" <*> pExp3
     <|>  id <$> pExp4
        where f a _ c = And a c

pExp4 :: Parser Exp
pExp4 =  f <$> pExp5 <*> token' "==" <*> pExp4
     <|> g <$> pExp5 <*> symbol' '<' <*> pExp4
     <|> h <$> pExp5 <*> symbol' '>' <*> pExp4
     <|> id <$> pExp5
        where f a _ c = EqualsTo a c
              g a _ c = LessThen a c
              h a _ c = MoreThen a c

pExp5 :: Parser Exp
pExp5 = id <$> pExp6

pExp6 :: Parser Exp
pExp6 =  f <$> pExp7 <*> symbol' '+' <*> pExp6
     <|> g <$> pExp7 <*> symbol' '-' <*> pExp6
     <|> id <$> pExp7
        where f a _ c = Add a c
              g a _ c = Sub a c

pExp7 :: Parser Exp
pExp7 =  f <$> pExp8 <*> symbol' '*' <*> pExp7
     <|> id <$> pExp8
        where f a _ c = Mul a c

pExp8 :: Parser Exp
pExp8 = id <$> pExp9

pExp9 :: Parser Exp
pExp9 = id <$> pFactor

pFactor :: Parser Exp
pFactor =  f   <$> number
       <|> Var <$> ident
       <|> g   <$> enclosedBy (symbol' '(')
                              pExp
                              (symbol' ')')
       where f a = Const (read a)
             g a = a