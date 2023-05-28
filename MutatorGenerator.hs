{-# LANGUAGE DeriveDataTypeable #-}

module MutatorGenerator where

import Ast
import Data.Data
import Data.Generics.Zipper
import LanguageParser
import LanguageUnparser
import Tests
import TestGenerator
import Test.QuickCheck
import Library.StrategicData (StrategicData)
import Library.Ztrategic
import StratProg



import System.IO.Unsafe
-- Catalogo de mutantes exp

genMutation :: Gen Mutation
genMutation = elements [TwoExpMut,OneExpMut,ConstMut,VarMut,BooleanMut]

mutateTwoExp :: Exp -> Exp -> Gen Exp
mutateTwoExp e1 e2 = elements[Add e1 e2,Sub e1 e2, Mul e1 e2, Div e1 e2,EqualsTo e1 e2,
 Or e1 e2, And e1 e2, LessThen e1 e2, MoreThen e1 e2, LessEqualThen e1 e2, MoreEqualThen e1 e2]

mutateConstExp :: Int -> Gen Exp
mutateConstExp val = elements[Const (val-2),Const (val-1),Const (val+1),Const (val+2)]


randomStringGen :: Int -> Gen String
randomStringGen 0 = return ""
randomStringGen size = do
  newVal <- elements ['a'..'z']
  rest <- randomStringGen (size - 1)
  return (newVal : rest)

mutateVarExp :: Int -> Gen Exp
mutateVarExp size = do
    newVal <- randomStringGen size
    return (Var newVal)

generate' = unsafePerformIO.generate


-- Mutar arvore com mutações inseridas
{-
mutationOp :: Exp -> Mutation -> Maybe Exp
mutationOp exp TwoExpMut = case exp of
    Add e1 e2           -> Just $ generate' $ mutateTwoExp e1 e2
    Sub e1 e2           -> Just $ generate' $ mutateTwoExp e1 e2
    Mul e1 e2           -> Just $ generate' $ mutateTwoExp e1 e2
    Div e1 e2           -> Just $ generate' $ mutateTwoExp e1 e2
    EqualsTo e1 e2      -> Just $ generate' $ mutateTwoExp e1 e2
    Or e1 e2            -> Just $ generate' $ mutateTwoExp e1 e2
    And e1 e2           -> Just $ generate' $ mutateTwoExp e1 e2
    LessThen e1 e2      -> Just $ generate' $ mutateTwoExp e1 e2
    MoreThen e1 e2      -> Just $ generate' $ mutateTwoExp e1 e2
    LessEqualThen e1 e2 -> Just $ generate' $ mutateTwoExp e1 e2
    MoreEqualThen e1 e2 -> Just $ generate' $ mutateTwoExp e1 e2
mutationOp exp OneExpMut = case exp of
    Not e           -> Just e
mutationOp exp ConstMut = case exp of
    Const val           -> Just $ generate' $ mutateConstExp val
mutationOp exp VarMut = case exp of
    Var val           -> Just $ generate' $ mutateVarExp $ length $ val
mutationOp exp BooleanMut = case exp of
    Boolean True -> Just (Boolean False)
    Boolean False -> Just (Boolean True) 
mutationOp _ _ = Nothing
-}

mutateExp :: Exp -> Gen Exp
mutateExp (Sub e1 e2) = mutateTwoExp e1 e2
mutateExp (Mul e1 e2) = mutateTwoExp e1 e2
mutateExp (Div e1 e2) = mutateTwoExp e1 e2
mutateExp (EqualsTo e1 e2) = mutateTwoExp e1 e2
mutateExp (Or e1 e2) = mutateTwoExp e1 e2
mutateExp (And e1 e2) = mutateTwoExp e1 e2
mutateExp (LessThen e1 e2) = mutateTwoExp e1 e2
mutateExp (MoreThen e1 e2) = mutateTwoExp e1 e2
mutateExp (LessEqualThen e1 e2) = mutateTwoExp e1 e2
mutateExp (MoreEqualThen e1 e2) = mutateTwoExp e1 e2
mutateExp (Not e) = return e
mutateExp (Const val) = mutateConstExp val
mutateExp (Var val) = mutateVarExp $ length $ val
mutateExp (Boolean True) = return (Boolean False)
mutateExp (Boolean False) = return (Boolean True)
mutateExp e = return e


--Escolhe uma expressão para mutar
produceMutationExpProgram :: Program -> Gen(Exp,Exp)
produceMutationExpProgram program = do
    chosen <- elements (getExps program)
    mutatedExp <- mutateExp chosen
    return (chosen,mutatedExp)

-- Agrega exps numa lista
getExps :: Program -> [Exp]
getExps code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` groupExp
        in 
        newCode

groupExp:: Exp -> Maybe [Exp]
groupExp e = Just [e]