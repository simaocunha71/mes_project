module Exp where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast

-- "3 + 4 * 2"
e :: Exp
e = Add (Const 3) (Mul (Const 4) (Const 2))

{-
     Precedence levels
 () [] -> . ++ --                    left to right   [1]
       ! ~ ++ -- + - (type) * & sizeof     right to left   [2]
       * / %                               left to right
       + -                                 left to right
       << >>                               left to right
       < <= > >=                           left to right
       == !=                               left to right
       &                                   left to right
       ^                                   left to right
       |                                   left to right
       &&                                  left to right
       ||                                  left to right
       ?:                                  right to left
       = += -= *= /= %= <<= >>= &= ^= |=   right to left
       ,                                   left to right
(pode expandir mais)
Exp  -> spaces Exp1

Exp1 -> Exp2 "||" Exp1
     |  Exp2

Exp2 -> Exp3 "&&" Exp2
     |  Exp3

Exp3 -> Exp4 "==" Exp3
     |  Exp4

Exp4 -> Exp5 '<' Exp4
     |  Exp5 '>' Exp4
     |  Exp5

Exp5 -> Exp6 '+' Exp5
     |  Exp6 '-' Exp5
     |  Exp6

Exp5 -> Exp6 '*' Exp5
     |  Exp6 '/' Exp5
     |  Exp6

pFactor -> int
         |  true
         |  false
         |  var
         |  '('  Exp ')'

-}


pExp :: Parser Exp
pExp = f <$> spaces <*> pExp1
     where f a b = b

pExp1 :: Parser Exp
pExp1 =  f <$> pExp2 <*> token' "||" <*> pExp1
     <|> id <$> pExp2
        where f a _ c = Or a c
        
pExp2 :: Parser Exp
pExp2 = f <$> pExp3 <*> token' "&&" <*> pExp2
     <|>  id <$> pExp3
        where f a _ c = And a c

pExp3 :: Parser Exp
pExp3 =  f <$> pExp4 <*> token' "==" <*> pExp3
     <|> id <$> pExp4
        where f a _ c = EqualsTo a c

pExp4 :: Parser Exp
pExp4 =  f <$> pExp5 <*> symbol' '<' <*> pExp4
     <|> g <$> pExp5 <*> symbol' '>' <*> pExp4
     <|> id <$> pExp5
     where f a _ c = LessThen a c
           g a _ c = MoreThen a c

pExp5 :: Parser Exp
pExp5 =  f <$> pExp6 <*> symbol' '+' <*> pExp5
     <|> g <$> pExp6 <*> symbol' '-' <*> pExp5
     <|> id <$> pExp6
        where f a _ c = Add a c
              g a _ c = Sub a c

pExp6 :: Parser Exp
pExp6 =  f <$> pFactor <*> symbol' '*' <*> pExp6
     <|> g <$> pFactor <*> symbol' '/' <*> pExp6
     <|> id <$> pFactor
        where f a _ c = Mul a c
              g a _ c = Div a c

pFactor :: Parser Exp
pFactor =  f   <$> number
       <|> j1  <$> pTrue
       <|> j2  <$> pFalse
       <|> g   <$> ident 
       <|> h   <$> (symbol' '(') <*> pExp2 <*> (symbol' ')')
       where 
            f r1 = Const (read r1)
            j1 _ =  Boolean True
            j2 _ =  Boolean False
            g r1 = Var r1
            h r1 r2 r3 = r2
