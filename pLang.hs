module PLang where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast
import Exp


teste_1 = "f(altura){int x = 2;}"
teste_2 =  "f(altura){if(x>2){}}"

pLang = pFuncs 

pFuncs :: Parser [Func]
pFuncs = f <$> ident <*> symbol' '(' <*> pIds <*> symbol' ')' <*> symbol' '{' <*> pStatements <*> symbol' '}'
        where 
          f r1 r2 r3 r4 r5 r6 r7 = [FDeclare r1 r3 r6]

pIds :: Parser [String]
pIds =    f <$> ident <*> symbol' ',' <*> pIds
      <|> g <$> ident 
      <|> succeed []
      where 
          f r1 r2 r3 = r1:r3
          g r1 = [r1]

pStatements :: Parser [Stat] 
pStatements =  f <$> pDecl <*> symbol' ';'  <*> pStatements
            <|> g <$> pIf <*> pStatements 
            <|> succeed []
              where 
                f r1 r2 r3 = r1:r3
                g r1 r2 = r1:r2


pDecl :: Parser Stat
pDecl = f <$>  token' "int"  <*> ident <*> symbol' '=' <*> pExp
        <|> g <$>  token' "string"  <*> ident <*> symbol' '=' <*> pExp
        where
          f r1 r2 r3 r4 = Declare Int r2 r4
          g r1 r2 r3 r4 = Declare String r2 r4

pBloco :: Parser [Stat]
pBloco = f <$> symbol' '{' <*>  pStatements <*> symbol' '}'
        where 
            f r1 r2 r3 = r2

pIf :: Parser Stat
pIf = f <$> token' "if" <*> symbol' '(' <*> pCond <*> symbol' ')' <*> pBloco
    <|> g <$> token' "if" <*> symbol' '(' <*> pCond <*> symbol' ')' <*> pBloco <*> token' "else"  <*> pBloco
    where 
        f r1 r2 r3 r4 r5  = ITE r3 r5 []
        g r1 r2 r3 r4 r5 r6 r7 = ITE r3 r5 r7 



pCond :: Parser Exp
pCond =  a  <$> pNestedCond <*> token' "==" <*> pCond
          <|> b  <$> pNestedCond <*> token' "||" <*> pCond
          <|> c  <$> pNestedCond <*> token' "&&" <*> pCond
          <|> d  <$> pNestedCond <*> token' "<" <*> pCond
          <|> e  <$> pNestedCond <*> token' ">" <*> pCond
          <|> f  <$> pNestedCond
        where a x _ z = EqualsTo x z
              b x _ z = Or x z
              c x _ z = And x z
              d x _ z = LessThen x z
              e x _ z = MoreThen x z
              f x = x
 
pNestedCond :: Parser Exp
pNestedCond =  a <$> pExp
                <|> b <$> enclosedBy (symbol' '(')
                                      pCond
                                     (symbol' ')')
                where a x = Exp x
                      b x = x