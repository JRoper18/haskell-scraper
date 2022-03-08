-- module Main where

-- import Lib

-- main :: IO ()
-- main = someFunc


{-# LANGUAGE CPP #-}

import System.Environment
import Lib
import Parsed

main :: IO ()
main = do
  args <- getArgs
  let outF = head args
  let inFs = tail args
  mapM_ (processSourceFile outF) inFs
 