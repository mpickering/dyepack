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
  -- This function will break the debugger if `a` is retained at this
  -- point
  checkDyed dyed (\x -> print "LEAKED: PAUSING")
  -- y retains a reference to a, so it can't be gced
  print y

