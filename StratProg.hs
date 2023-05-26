{-# LANGUAGE DeriveDataTypeable #-}

module StratProg where

import Ast
import Data.Data
import Data.Generics.Zipper
import LanguageParser
import LanguageUnparser
import Tests


import Library.StrategicData (StrategicData)
import Library.Ztrategic

instance StrategicData Program
instance StrategicData Exp
instance StrategicData Stat
instance StrategicData Type
instance StrategicData Par
instance StrategicData Func
instance StrategicData a => StrategicData [a]

-- ## Optimazations

-- Aplica a otmização do elemento neutro das operações à AST
applyOptimizations :: Program -> Program
applyOptimizations code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost step ) codeZipper
            where step = failTP `adhocTP` expNeutralOp `adhocTP` loopImprove
        in 
        fromZipper newCode

-- Expressa o elemento neutro das operações
expNeutralOp :: Exp -> Maybe Exp
expNeutralOp (Add e (Const 0)) = Just e
expNeutralOp (Add (Const 0) t) = Just t 
expNeutralOp (Mul e (Const 1)) = Just e
expNeutralOp (Mul (Const 1) t) = Just t
expNeutralOp _ = Nothing

-- Aumenta a percepção dos loops
loopImprove :: Stat -> Maybe Stat
loopImprove (While (Const 0) st) = Just (While (Boolean False) st)
loopImprove (While (Const _) st) = Just (While (Boolean True) st)
loopImprove (For r1 (Const 0) r3 r4) = Just (For r1 (Boolean False) r3 r4)
loopImprove (For r1 (Const _) r3 r4) = Just (For r1 (Boolean True) r3 r4)
loopImprove (For [] r2 [] r4) = Just (While r2 r4)
loopImprove (For r1 r2 r3 r4) = Just (Sequence (r1 ++ [(While r2 (r3 ++ r4))]))
loopImprove _ = Nothing

-- ## Smells

-- Identifica e faz refctoring de smells
smellRefactor :: Program -> Program
smellRefactor code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTP (innermost  step ) codeZipper
            where step = failTP `adhocTP` booleanLiterals `adhocTP` ifNot
        in 
        fromZipper newCode

-- Refactor smell: if ( f1() ) { return True } else {return False}
booleanLiterals :: Stat -> Maybe Stat
booleanLiterals (ITE (ExpFunctionCall name args) [Return (Boolean _)] [Return (Boolean _)]) = Just (Return (ExpFunctionCall name args))
booleanLiterals _ = Nothing

-- Refactor smell: if (not …) { b1 } else { b2 } ⇒ if (..) { b2 } else { b1 }
ifNot :: Stat -> Maybe Stat
ifNot (ITE (Not exp) cod1 cod2) = Just (ITE (exp) cod2 cod1)
ifNot _ = Nothing

-- -- Identifica smells
-- smellDetector :: Program -> [String]
-- smellDetector code = 
--     let codeZipper = toZipper code
--         (Just newCode) = applyTU (innermost  step ) codeZipper
--             where step = failTU ........
--         in 
--         fromZipper newCode



-- ## Bugs

-- Agrega bugs numa lista
bugTrack :: Program -> [String]
bugTrack code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` subZero
        in 
        newCode

subZero:: Exp -> Maybe [String]
subZero (Div e (Const 0)) = Just ["subZero"]
subZero _ = Nothing

----

                    -- Function name, Vars declared, Vars used
gatherData :: Program -> [(String,[String],[String])]
gatherData  code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` varsDeclaration
        in 
        newCode

varsDeclaration :: Func -> Maybe [(String,[String],[String])]
varsDeclaration (FunctionDeclaration _ funcName _ stats) = Just [(funcName,gatherDecls stats,gatherUsed stats)]
varsDeclaration _ = Just []


gatherDecls :: [Stat] -> [String]
gatherDecls code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` groupDecls
        in 
        newCode

gatherUsed :: [Stat] -> [String]
gatherUsed code = 
    let codeZipper = toZipper code
        (Just newCode) = applyTU (full_tdTU  step ) codeZipper
            where step = failTU `adhocTU` groupUsed
        in 
        newCode


groupDecls :: Stat -> Maybe [String]
groupDecls (Declare _ name) = Just [name]
groupDecls (DeclAssign _ name _) = Just [name]
groupDecls _ = Just []

groupUsed :: Exp -> Maybe [String]
groupUsed (Var name) = Just [name]
groupUsed _ = Just []

