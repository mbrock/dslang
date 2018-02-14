module Dslang where

import System.Environment (getArgs)

import Dslang.Base
import Dslang.Read
import Dslang.Grok
import Dslang.Emit

import EVM.Assembly (compile)
import qualified EVM.Symbex.Main as Symbex

main :: IO ()
main = do
  [path] <- getArgs
  x <- parseFromFile path
  case x of
    Just y -> do
      print y
      putStrLn ""
      case grok y of
        Left e -> error (show e)
        Right z -> do
          mapM_ (printf "%02x") (compile (emit z))
          putStrLn ""
    Nothing ->
      pure ()

dcs :: IO ()
dcs = do
  x <- parseFromFile "example.ds"
  case x of
    Just y -> do
      print y
      putStrLn ""
      case grok y of
        Left e -> error (show e)
        Right z -> do
          mapM_ (printf "%02x") (compile (emit z))
          Symbex.showPaths (Symbex.run (emit z))
          putStrLn ""
    Nothing ->
      pure ()
