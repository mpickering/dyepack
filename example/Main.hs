{-# LANGUAGE DeriveGeneric #-}

module Main where

import Debug.Dyepack
import GHC.Generics
import GHC.Debug.Stub

data A = A String deriving Show
data User = User A Int deriving Generic


main = do
  start
  v <- getLine
  let a = A v
      y = id [a, a, a]
      u = User a 5

  dyed <- dye u
  checkDyed dyed (\x -> saveClosure x >> pause)
  print y

