{-# LANGUAGE DeriveGeneric #-}

module Main where

import Debug.Dyepack
import GHC.Generics

data A = A String deriving Show
data User = User A Int deriving Generic

main = do
  v <- getLine
  let a = A v
      y = id [a, a, a]
      u = User a 5

  dyed <- dye u
  checkDyed dyed
  print y

