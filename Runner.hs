module Runner where

import LanguageParser
import LanguageUnparser
import StratProg
import Ast
import Tests
import TestGenerator
import Test.QuickCheck


-- Normal parsing
parse :: String -> Program
parse s = fst $ head $ pProgram s

unparse :: Program -> String
unparse p = showProgram p

-- Optimazations
parseWithOpt :: String -> Program
parseWithOpt s = (applyOptimizations(parse s))

applyOpts:: Program -> Program
applyOpts p = (applyOptimizations p)

-- Handling smells
parseWithRefactor :: String -> Program
parseWithRefactor s = smellRefactor $ parse s

applyRefactor :: Program -> Program
applyRefactor p = smellRefactor p

-- Handling bugs
bugCount :: String -> Int
bugCount s = length $ bugTrack  $ parse s

-- Automated Test Case Generation
autoTestCaseGen :: IO Program
autoTestCaseGen = generate genProgram

unparseAutoTestCase :: IO Program ->  IO String
unparseAutoTestCase ioProg = ioProg >>= return . unparse


