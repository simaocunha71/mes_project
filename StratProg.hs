{-# LANGUAGE DeriveDataTypeable #-}

module StratProg where

import Ast
import Data.Data
import Data.Generics.Zipper
import Exp
import Parser

import Library.StrategicData (StrategicData)
import Library.Ztrategic

instance StrategicData Exp
instance StrategicData Stat
instance StrategicData Type
instance StrategicData Par
instance StrategicData Func
instance StrategicData a => StrategicData [a]

--Escrever aqui as funçoes de strategic programming