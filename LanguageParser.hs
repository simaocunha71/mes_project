module LanguageParser where

import Prelude hiding ((<*>),(<$>))
import Parser
import Ast
import Exp
import Tests

-- Gramática
{-

Program  -> zeroOrMore Func

Func -> spaces Type '(' Parameters ')' '{' Statements '}'

Parameters -> Parameter ',' Parameters
           | empty

Parameter -> Type

Type -> int
     |  char
     |  string
     |  void

Statements -> Decl ';' Statements
           |  Assign ';' Statements
           |  If Statements
           |  FuncCall ';' Statements
           |  While Statements
           |  For Statements
           |  empty

Decl -> Declare
     |  DeclareAssign

Declare -> Type

DeclareAssign -> Type '=' Exp

Assign -> '=' Exp

If -> "if" '(' Cond ')' Bloco
   |  "if" '(' Cond ')' Bloco "else" Bloco

FuncCall -> '(' Args ')'

While -> "while" '(' Cond ')' Bloco

Cond -> NestedCond "==" Cond
     |  NestedCond "||" Cond
     |  NestedCond "&&" Cond
     |  NestedCond "<"  Cond
     |  NestedCond ">"  Cond
     |  NestedCond "<="  Cond
     |  NestedCond ">="  Cond
     |  "!" Cond
     |  NestedCond

NestedCond -> Exp
           |  '(' Cond ')'

For -> "for" '(' (zeroOrMore Decl) ';' Cond ';' (zeroOrMore Assign) ')' Bloco

Bloco -> '{' Statements '}'

-}

pProgram :: Parser Program
pProgram = f <$> ( zeroOrMore pFunc )
        where 
          f r1 = Prog r1

pFunc :: Parser Func
pFunc = f <$> spaces <*> pType <*> ident <*> symbol' '(' <*> pParameters <*> symbol' ')' <*> symbol' '{' <*> newlines <*> pStatements <*> symbol' '}'
        where 
          f r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 = FunctionDeclaration r1 r2 r4 r8

pIds :: Parser [String]
pIds =    f <$> ident <*> symbol' ',' <*> pIds
      <|> g <$> ident 
      <|> succeed []
      where 
          f r1 r2 r3 = r1:r3
          g r1 = [r1]

pType :: Parser Type
pType = f <$>  token' "int"
      <|> g <$> token' "char"
      <|> h <$> token' "string"
      <|> i <$> token' "void"
      where
        f r1 = Int 
        g r1 = Char 
        h r1 = String
        i r1 = Void

pParameters :: Parser [Par]
pParameters = f <$> pParameter 
            <|> g <$> pParameter <*> symbol' ','  <*> pParameters
            <|> succeed []
            where 
              f r1 = [r1]
              g r1 r2 r3  = r1:r3

pParameter :: Parser Par
pParameter = f <$>  pType <*> ident 
          where
            f r1 r2 = Parameter r1 r2

pStatements :: Parser [Stat] 
pStatements =  f <$> pDecl <*> symbol' ';' <*> newlines <*> pStatements
            <|> f1 <$> pAssign <*> symbol' ';'  <*> newlines  <*> pStatements
            <|> g <$> pIf  <*> newlines <*>  pStatements 
            <|> h <$> pFuncCall <*> symbol' ';' <*> newlines  <*> pStatements
            <|> i <$> pWhile  <*> newlines <*> pStatements
            <|> j <$> pFor  <*> newlines <*> pStatements 
            <|> k <$> pReturn 
            <|> succeed []
              where 
                f r1 r2 r3 r4  = r1:r4
                f1 r1 r2 r3 r4 = r1:r4
                g r1 r2 r3     = r1:r3
                h r1 r2 r3 r4  = r1:r4
                i r1 r2 r3    = r1:r3
                j r1 r2 r3    = r1:r3
                k r1 = [r1]

pFuncCall :: Parser Stat 
pFuncCall = f <$> ident <*> (symbol' '(') <*>  pArgs  <*> (symbol' ')') 
            where 
                f r1 r2 r3 r4 = FunctionCall r1 r3

pExpFuncCall :: Parser Exp 
pExpFuncCall = f <$> ident <*> (symbol' '(') <*>  pArgs  <*> (symbol' ')') 
            where 
                f r1 r2 r3 r4  = ExpFunctionCall r1 r3

pArgs :: Parser [Exp]
pArgs = f <$> pExp <*> (symbol' ',')  <*> pArgs
    <|> g <$> pExp 
    <|> succeed []
    where
      f r1 r2 r3 = r1:r3
      g r1 = [r1]


pIf :: Parser Stat
pIf = f <$> token' "if" <*> symbol' '(' <*> pCond <*> symbol' ')' <*> pBloco
    <|> g <$> token' "if" <*> symbol' '(' <*> pCond <*> symbol' ')' <*> pBloco <*> token' "else"  <*> pBloco
    where 
        f r1 r2 r3 r4 r5  = ITE r3 r5 []
        g r1 r2 r3 r4 r5 r6 r7 = ITE r3 r5 r7 

pDecl :: Parser Stat
pDecl =  pDeclare
     <|> pDeclareAssign

pDeclare :: Parser Stat
pDeclare = f <$>  pType  <*> ident
        where
          f r1 r2 = Declare r1 r2

pDeclareAssign :: Parser Stat
pDeclareAssign = f <$>  pType  <*> ident <*> symbol' '=' <*> pExp
            where
              f r1 r2 r3 r4 = DeclAssign r1 r2 r4

pAssign :: Parser Stat
pAssign =  f <$> ident <*> symbol' '=' <*> pExp
       where
          f r1 r2 r3 = Assign r1 r3

pForDeclVars :: Parser [Stat]
pForDeclVars = f <$> pDecl
              <|> g <$> pDecl <*> symbol' ',' <*> pForDeclVars
              <|> succeed []
            where 
              f r1 = [r1] 
              g r1 r2 r3 = r1:r3

pForAssigns :: Parser [Stat]
pForAssigns = f <$> pAssign
              <|> g <$> pAssign <*> symbol' ',' <*> pForAssigns
              <|> succeed []
            where 
              f r1 = [r1] 
              g r1 r2 r3 = r1:r3
              

pBloco :: Parser [Stat]
pBloco = f <$> symbol' '{' <*>  pStatements <*> symbol' '}'
        where 
            f r1 r2 r3 = r2

pReturn :: Parser Stat
pReturn = f <$> token' "return" <*> pExp <*> symbol' ';'
        where
          f r1 r2 r3 = Return r2


pCond :: Parser Exp
pCond =  a0 <$> pExpFuncCall <*> symbol' ';'
     <|> a  <$> pNestedCond  <*> token' "==" <*> pCond
     <|> b  <$> pNestedCond  <*> token' "||" <*> pCond
     <|> c  <$> pNestedCond  <*> token' "&&" <*> pCond
     <|> d  <$> pNestedCond  <*> token' "<"  <*> pCond
     <|> e  <$> pNestedCond  <*> token' ">"  <*> pCond
     <|> f  <$> token' "!"   <*> pCond
     <|> g  <$> pNestedCond
        where a0 r1 r2 = r1
              a x _ z = EqualsTo x z
              b x _ z = Or x z
              c x _ z = And x z
              d x _ z = LessThen x z
              e x _ z = MoreThen x z
              f x y   = Not y 
              g x = x
 
pNestedCond :: Parser Exp
pNestedCond =  a <$> pExp
                <|> b <$> enclosedBy (symbol' '(')
                                      pCond
                                     (symbol' ')')
                where a x = x
                      b x = x

pWhile :: Parser Stat
pWhile = f <$> token' "while" <*> symbol' '(' <*> pCond <*> symbol' ')' <*> pBloco
      where 
        f r1 r2 r3 r4 r5 = While r3 r5

pFor :: Parser Stat
--               for              (           int x = 0          ;      i < 0              ;      i = i + 1            )      [bloco de cenas]
pFor = f <$> token' "for" <*> symbol' '(' <*> pForDeclVars <*> symbol' ';' <*> pCond <*> symbol' ';' <*> pForAssigns <*> symbol' ')' <*> pBloco
    where 
      f r1 r2 r3 r4 r5 r6 r7 r8 r9 = For r3 r5 r7 r9