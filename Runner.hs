module Runner where

import LanguageParser
import LanguageUnparser
import StratProg
import Ast
import Tests


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



