module Runner where

import LanguageParser
import LanguageUnparser
import StratProg
import Ast
import Tests


parse :: String -> Program
parse s = fst $ head $ pProgram s

unparse :: Program -> String
unparse p = showProgram p

parseWithOpt :: String -> Program
parseWithOpt s = (applyOptimizations(parse s))

applyOpts:: Program -> Program
applyOpts p = (applyOptimizations p)