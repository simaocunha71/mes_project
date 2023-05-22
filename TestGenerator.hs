module TestGenerator where

import Ast
import Test.QuickCheck
import Data.Char



-- Generator for Type
genType :: Gen Type
genType = elements [Int, Char, String, Void]


genStr :: Gen String
genStr = do
  -- Generate a random length between 0 and 10
  length <- choose (0, 10)
  -- Generate a list of random characters from 'a' to 'z' and 'A' to 'Z'
  chars <- vectorOf length $ elements (['a'..'z'] ++ ['A'..'Z'])
  return chars

genBool :: Gen Bool
genBool =  elements [True,False]

genInt :: Gen Int
genInt = choose (0, 100)

-- Generator for Exp
genExp :: Gen Exp
genExp = oneof
  [ Add <$> genExp <*> genExp
  , Sub <$> genExp <*> genExp
  , Mul <$> genExp <*> genExp
  , Div <$> genExp <*> genExp
  , Not <$> genExp
  , Const <$> genInt
  , Var <$> genStr
  , Boolean <$> genBool
  , EqualsTo <$> genExp <*> genExp
  , Or <$> genExp <*> genExp
  , And <$> genExp <*> genExp
  , LessThen <$> genExp <*> genExp
  , MoreThen <$> genExp <*> genExp
  , LessEqualThen <$> genExp <*> genExp
  , MoreEqualThen <$> genExp <*> genExp
  , ExpFunctionCall <$> genStr <*> vectorOf 1 genExp
  ]

-- Generator for Par
genPar :: Gen Par
genPar = Parameter <$> genType <*> genStr

-- Generator for Stat
genStat :: Gen Stat
genStat = oneof
  [ Assign <$> genStr <*> genExp
  , Declare <$> genType <*> genStr
  , DeclAssign <$> genType <*> genStr <*> genExp
  , ITE <$> genExp <*> vectorOf 1  genStat <*> vectorOf 1  genStat
  , While <$> genExp <*> vectorOf 1  genStat
  , For <$> vectorOf 1 genStat <*> genExp <*> vectorOf 1  genStat <*> vectorOf 1  genStat
  , FunctionCall <$> genStr <*> vectorOf 1 genExp
  , Sequence <$> vectorOf 1  genStat
  , Return <$> genExp
  ]

-- Generator for Func
genFunc :: Gen Func
genFunc = FunctionDeclaration <$> genType <*> genStr <*> vectorOf 1 genPar <*> vectorOf 1  genStat


-- Generator for Program
genProgram :: Gen Program
genProgram = Prog <$> vectorOf 1  genFunc


