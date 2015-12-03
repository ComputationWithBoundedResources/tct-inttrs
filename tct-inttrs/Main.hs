-- | This module provides the default executable tct-inttrs.
module Main (main) where

import Tct.Core
import Tct.Trs
import Tct.IntTrs

instance Declared Trs Trs       where decls = trsDeclarations
instance Declared IntTrs IntTrs where decls = intTrsDeclarations

main :: IO ()
main = runIntTrs intTrsConfig

