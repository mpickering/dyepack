{-# LANGUAGE DeriveGeneric #-}

module Main where

import Debug.Dyepack
import GHC.Generics

data Foo = Foo Int String Bar
  deriving Generic

data Bar = Bar Float

data A = A String deriving Show
data User = User A Int deriving Generic

main = do
  v <- getLine
  let x = A v
  let y = id [x,x,x]
      u = User x 5

  dyed <- mkDyed u
  checkDyed dyed
  print y

