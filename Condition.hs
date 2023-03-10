module Condition where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast
import Exp

-- "4 == 2"
c :: Condition
c = EqualsTo (Exp(Const 4))  (Exp(Const 2))

-- Gramática
-- Condition -> Condition '==' Condition
--           |  Condition '||' Condition
--           |  Condition '&&' Condition
--           |  Exp

pCondition :: Parser Condition
pCondition =  a  <$> pNestedCondition <*> token' "==" <*> pNestedCondition
          <|> b  <$> pNestedCondition <*> token' "||" <*> pNestedCondition
          <|> c  <$> pNestedCondition <*> token' "&&" <*> pNestedCondition
          <|> d  <$> pNestedCondition <*> token' "<" <*> pNestedCondition
          <|> e  <$> pNestedCondition <*> token' ">" <*> pNestedCondition
          <|> f  <$> pNestedCondition
        where a x _ z = EqualsTo x z
              b x _ z = Or x z
              c x _ z = And x z
              d x _ z = LessThen x z
              e x _ z = MoreThen x z
              f x = x
 
pNestedCondition :: Parser Condition
pNestedCondition =  a <$> pExp
                <|> b <$> enclosedBy (symbol' '(')
                                      pCondition
                                     (symbol' ')')
                where a x = Exp x
                      b x = x