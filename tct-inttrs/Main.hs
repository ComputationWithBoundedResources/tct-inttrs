-- | This module provides the default executable tct-inttrs.
module Main (main) where


import Control.Monad (void)
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
      | p == "--validate" -> parseIO fp >>= either putError validate
      | p == "--putTrs"   -> parseIO fp >>= either putError (putFormat putTrs)
      | p == "--putIts"   -> parseIO fp >>= either putError (putFormat putIts)
      | otherwise         -> run
    _                     -> run
    where
      run            = runIntTrs intTrsConfig
      putUsage       = putStrLn "Validate IntTrs format: \n -- validate\nOutput in Trs/Its format:\n  --putTrs\toutput in Trs format\n  --putIts\toutput in Its format\n"
      putError err   = hPutStrLn stderr err >> exitFailure
      putFormat f rs = f (rules rs) >> exitSuccess
      validate rs    = either putError (void .return) (isWellFormed $ rules rs)

