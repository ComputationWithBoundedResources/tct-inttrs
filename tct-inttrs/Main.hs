-- | This module provides the default executable tct-inttrs.
module Main (main) where


import System.Environment (getArgs)
import System.IO          (hPutStrLn, stderr)
import System.Exit        (exitFailure, exitSuccess)

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
    ["--help"]          -> putUsage >> run
    (p:fp:_)
      | p == "--putTrs" -> parseIO fp >>= either putError (putFormat putTrs)
      | p == "--putIts" -> parseIO fp >>= either putError (putFormat putIts)
      | otherwise       -> run
    _                   -> run
    where
      run            = runIntTrs intTrsConfig
      putUsage       = putStrLn "Output in Trs/Its format:\n  --putTrs\toutput in Trs format\n  --putIts\toutput in Its format\n"
      putError err   = hPutStrLn stderr err >> exitFailure
      putFormat f rs = f (rules rs) >> exitSuccess

