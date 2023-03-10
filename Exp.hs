module Exp where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast

-- "3 + 4 * 2"
e :: Exp
e = Add (Const 3) (Mul (Const 4) (Const 2))

-- Gramática
-- Exp    -> Termo '+' Exp
--        |  Termo
-- Termo  -> Factor '*' Termo
--        |  Factor
-- Factor -> int
--        |  var
--        |  '('  Exp ')'


pExp :: Parser Exp
pExp =  f  <$> pTermo <*> symbol' '+' <*> pExp
    <|> id <$> pTermo
    where f a _ c = Add a c
          

pTermo :: Parser Exp
pTermo =  f  <$> pFactor <*> symbol' '*' <*> pTermo
      <|> id <$> pFactor
    where f a _ c = Mul a c
 

pFactor :: Parser Exp
pFactor =  f   <$> number
       <|> Var <$> ident
       <|> g   <$> enclosedBy (symbol' '(')
                              pExp
                              (symbol' ')')
       where f a = Const (read a)
             g a = a