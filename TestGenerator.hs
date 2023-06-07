module TestGenerator where

import Ast
import Test.QuickCheck
import Data.Char


-- Generator for Type
genType :: Gen Type
genType = elements [Int, Char, String]

genFuncType :: Gen Type
genFuncType = elements [Int, Char, String, Void]

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
genParams :: [String] -> Gen [Par]
genParams [] = return []
genParams (h:t) = do
  parType <- genType
  parTail <- genParams t
  return (Parameter parType h : parTail)



genExps :: [String] -> Int -> Gen [Exp]
genExps usedNames maxExpDepth = do
  numExp <- choose (1, maxExpDepth)
  vectorOf 2 (genExpWithDepth usedNames numExp)

-- Generator for Exp
genExpWithDepth :: [String] -> Int -> Gen Exp
genExpWithDepth usedNames depth = do 
  -- Can only used declared vars in expressions
  if null usedNames
    then 
      if depth <= 1 
        then  oneof [Const <$> genInt,Boolean <$> genBool]
      else  oneof
        [  Add <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  Sub <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  Mul <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  Div <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  Not <$> genExpWithDepth usedNames (depth - 1)
        ,  Const <$> genInt
        ,  Boolean <$> genBool
        ,  EqualsTo <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  Or <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  And <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  LessThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  MoreThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  LessEqualThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ,  MoreEqualThen <$> genExpWithDepth usedNames (depth - 1) <*> genExpWithDepth usedNames (depth - 1)
        ]
    else if depth <= 1 
      then  
        oneof [Const <$> genInt,Var <$> elements usedNames,Boolean <$> genBool]
    else 
       oneof
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
  genStatWithDepth numStat usedNames maxNumStatements numExp

getNameFromStat :: Stat -> [String]
getNameFromStat stat = case stat of
  Assign name _ -> [name]
  Declare _ name -> [name]
  DeclAssign _ name _ -> [name]
  ITE _ _ _ -> []
  While _ _ -> []
  For _ _ _ _ -> []
  FunctionCall name _ -> [name]
  Sequence _ -> []
  Return _ -> []


genStatWithDepth :: Int -> [String] -> Int -> Int -> Gen [Stat]
genStatWithDepth 0 _ _ _ = return []
genStatWithDepth numStat usedNames maxNumStatements depth = do
  if (null usedNames) && (depth <= 1)
    then do
      stat <- oneof [Declare <$> genType <*> genUniqueName usedNames,  DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1]
      let updatedUsedNames = usedNames ++ (getNameFromStat stat)
      rest <- genStatWithDepth (numStat-1) updatedUsedNames maxNumStatements depth
      return (stat : rest)
    else if (null usedNames)
      then do
        stat <- oneof
          [ Declare <$> genType <*> genUniqueName usedNames
          , DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1
          , ITE <$> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
          , While <$> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
          , For <$> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
          , FunctionCall <$> elements usedNames <*> genExps usedNames depth
          , Return <$> genExpWithDepth usedNames 1
          , Sequence <$> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
          ]
        let updatedUsedNames = usedNames ++ (getNameFromStat stat)
        rest <- genStatWithDepth (numStat-1) updatedUsedNames maxNumStatements depth
        return (stat : rest)
    else if depth <= 1
      then do
        stat <- oneof [Assign <$> elements usedNames <*> genExpWithDepth usedNames 1, Declare <$> genType <*> genUniqueName usedNames, DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1]
        let updatedUsedNames = usedNames ++ (getNameFromStat stat)
        rest <- genStatWithDepth (numStat-1) updatedUsedNames maxNumStatements depth
        return (stat : rest)
    else do
      stat <- oneof
        [ Assign <$> elements usedNames <*> genExpWithDepth usedNames 1
        , Declare <$> genType <*> genUniqueName usedNames
        , DeclAssign <$> genType <*> genUniqueName usedNames <*> genExpWithDepth usedNames 1
        , ITE <$> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
        , While <$> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
        , For <$> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genExpWithDepth usedNames 1 <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1) <*> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
        , FunctionCall <$> elements usedNames <*> genExps usedNames depth
        , Return <$> genExpWithDepth usedNames 1
        , Sequence <$> genStatWithDepth (numStat - 1) usedNames maxNumStatements (depth - 1)
        ]
      let updatedUsedNames = usedNames ++ (getNameFromStat stat)
      rest <- genStatWithDepth (numStat-1) updatedUsedNames maxNumStatements depth
      return (stat : rest)


-- Generator for Func
genFunc :: Int -> Int -> Gen Func
genFunc maxNumStatements maxExpDepth = do
  funcName <- genStr
  numParams <- choose (0, 5)
  usedNames <- vectorOf numParams genStr
  functype <- genFuncType
  params <- genParams usedNames
  -- Number of statements to be generated
  numStatements <- choose (1, maxNumStatements)
  body <- genStat usedNames maxNumStatements maxExpDepth
  return  (FunctionDeclaration functype funcName params body)

-- Generator for Program
genProgram :: Int -> Int -> Int -> Gen Program
genProgram maxNumFuncs maxNumStatements maxExpDepth = do
  -- Number of functions to be generated
  numFuncs <- choose (1, maxNumFuncs)
  Prog <$> vectorOf numFuncs (genFunc maxNumStatements maxExpDepth)



