module Main (main) where

import Tct.Core
import Tct.Core.Data

import Tct.Trs

instance Declared Trs Trs where decls = [ SD competitionDeclaration, SD certifyDeclaration ]

main :: IO ()
main = runTrs trsConfig

