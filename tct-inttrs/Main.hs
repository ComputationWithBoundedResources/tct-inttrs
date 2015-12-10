-- | This module provides the default executable tct-inttrs.
module Main (main) where


import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)

import Tct.Core
import Tct.IntTrs
import Tct.Its
import Tct.Trs


instance Declared Trs Trs       where decls = trsDeclarations
instance Declared Its Its       where decls = itsDeclarations
instance Declared IntTrs IntTrs where decls = intTrsDeclarations

main :: IO ()
main = do
  args <- getArgs
  case args of
    (p:fp:_)
      | p == "--putTrs" -> parseIO fp >>= either (hPutStrLn stderr) (putTrs . rules)
      | p == "--putIts" -> parseIO fp >>= either (hPutStrLn stderr) (putIts . rules)
      | otherwise       -> runIntTrs intTrsConfig
    _                   -> runIntTrs intTrsConfig
