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
  length <- choose (1, 10)
  -- Generate a list of random characters from 'a' to 'z' and 'A' to 'Z'
  chars <- vectorOf length $ elements (['a'..'z'] ++ ['A'..'Z'])
  return chars

genBool :: Gen Bool
genBool =  elements [True,False]

genInt :: Gen Int
genInt = choose (0, 100)

-- Generator for strings with unique names
genUniqueName :: [String] -> Gen String
genUniqueName usedNames = do
  name <- genStr
  if name `elem` usedNames
    then genUniqueName usedNames
    else return name

-- Generator for Par
genPar :: [String] -> Gen Par
genPar usedNames = do
  parType <- genType
  parName <- genUniqueName usedNames
  return (Parameter parType parName)


genExps :: [String] -> Int -> Gen [Exp]
genExps usedNames maxExpDepth = do
  numExp <- choose (1, maxExpDepth)
  vectorOf 2 (genExpWithDepth usedNames numExp)

-- Generator for Exp
genExpWithDepth :: [String] -> Int -> Gen Exp
genExpWithDepth usedNames depth = do 
  if depth <= 1 then  oneof [Const <$> genInt,Var <$> elements usedNames,Boolean <$> genBool]
  else  oneof
    [  Add <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  Sub <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  Mul <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  Div <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  Not <$> genExpWithDepth usedNames (depth - 1)
    ,  Const <$> genInt
    ,  Var <$> elements usedNames
    ,  Boolean <$> genBool
    ,  EqualsTo <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  Or <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  And <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  LessThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  MoreThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  LessEqualThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  MoreEqualThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
    ,  ExpFunctionCall <$> elements usedNames <*> genExps usedNames (depth - 1)
    ]


-- Generator for Stat
genStat :: [String] -> Int -> Int ->  Gen [Stat]
genStat usedNames maxNumStatements maxExpDepth = do
  numStat <- choose (1, maxNumStatements)
  numExp <- choose (1, maxExpDepth)
  vectorOf numStat (genStatWithDepth usedNames maxNumStatements numExp )

genStatWithDepth :: [String] -> Int -> Int -> Gen Stat
genStatWithDepth usedNames maxNumStatements depth =
  if (null usedNames) && (depth <=1)
    then oneof [Declare <$> genType <*> genUniqueName usedNames,  DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1]
  else if (null usedNames )
    then 
      oneof
      [ Declare <$> genType <*> genUniqueName usedNames
      ,  DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1
      ,  ITE <$> genExpWithDepth usedNames  1 <*> genStat usedNames maxNumStatements (depth - 1) <*> genStat usedNames maxNumStatements (depth - 1)
      ,  While <$> genExpWithDepth usedNames 1  <*> genStat usedNames maxNumStatements (depth - 1)
      ,  For <$> genStat usedNames maxNumStatements (depth - 1) <*> genExpWithDepth usedNames 1<*> genStat usedNames maxNumStatements (depth - 1) <*>  genStat usedNames maxNumStatements (depth - 1)
      ,  FunctionCall <$> elements usedNames <*> genExps usedNames depth
      ,  Return <$> genExpWithDepth usedNames 1
      ,  Sequence <$> genStat  usedNames maxNumStatements (depth - 1)
      ]
  else if depth <=1 
    then oneof [ Assign <$> elements usedNames <*> genExpWithDepth usedNames 1,  Declare <$> genType <*> genUniqueName usedNames,  DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1]
  else oneof
      [  Assign <$> elements usedNames <*> genExpWithDepth usedNames 1
      ,  Declare <$> genType <*> genUniqueName usedNames
      ,  DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1
      ,  ITE <$> genExpWithDepth usedNames  1 <*> genStat usedNames maxNumStatements (depth - 1) <*> genStat usedNames maxNumStatements (depth - 1)
      ,  While <$> genExpWithDepth usedNames 1  <*> genStat usedNames maxNumStatements (depth - 1)
      ,  For <$> genStat usedNames maxNumStatements (depth - 1) <*> genExpWithDepth usedNames 1<*> genStat usedNames maxNumStatements (depth - 1) <*>  genStat usedNames maxNumStatements (depth - 1)
      ,  FunctionCall <$> elements usedNames <*> genExps usedNames depth
      ,  Return <$> genExpWithDepth usedNames 1
      ,  Sequence <$> genStat  usedNames maxNumStatements (depth - 1)
      ]

-- Generator for Func
genFunc :: Int -> Int -> Gen Func
genFunc maxNumStatements maxExpDepth = do
  funcName <- genStr
  numParams <- choose (0, 5)
  usedNames <- vectorOf numParams genStr
  let parGen = genPar usedNames
  params <- vectorOf numParams parGen
  -- Number of statements to be generated
  numStatements <- choose (1, maxNumStatements)
  body <- genStat usedNames maxNumStatements maxExpDepth
  return  (FunctionDeclaration Void funcName params body)

-- Generator for Program
genProgram :: Int -> Int -> Int -> Gen Program
genProgram maxNumFuncs maxNumStatements maxExpDepth = do
  -- Number of functions to be generated
  numFuncs <- choose (1, maxNumFuncs)
  Prog <$> vectorOf numFuncs (genFunc maxNumStatements maxExpDepth)



