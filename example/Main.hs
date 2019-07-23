{-# LANGUAGE DeriveGeneric #-}

module Main where

import Debug.Dyepack
import GHC.Generics
import GHC.Debug.Stub
import Control.Concurrent

data A = A String deriving Show
data User = User A Int deriving Generic

main = do
  start
  -- Give the debugger time to fire up
  v <- getLine
  let a = A v
      y = id [a, a, a]
      u = User a 5
  -- We want to make sure `a` is not retained
  saveClosures [Box a]
  dyed <- dye u
  -- This function will break the debugger if `a` is retained at this
  -- point
  checkDyed dyed (\x -> print "LEAKED: PAUSING" >> pause)
  -- y retains a reference to a, so it can't be gced
  print y

