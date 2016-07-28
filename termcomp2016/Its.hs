module Main (main) where

import Tct.Core
import Tct.Its

instance Declared Its Its where decls = [ SD $ strategy "competition" () runtime ]

main :: IO ()
main = runIts itsConfig

