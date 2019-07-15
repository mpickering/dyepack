{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE TypeApplications, FlexibleContexts #-}
{-# LANGUAGE BangPatterns, MagicHash #-}
module Debug.Dyepack (dye, checkDyed, Dyed) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.GGP
import GHC.Exts
import System.Mem.Weak
import System.Mem

foreign import ccall unsafe "findPtr" findPtr :: Ptr a -> Int -> IO ()

-- TODO: come up with a better name for Part
data Part = forall a. Part (Weak a)

-- | Represents an object who's contents on the heap have been "dyed".
-- The dyed contents have weak pointers, which can then be used to check if they
-- are being retained.
newtype Dyed a = Dyed [Part]

-- | Create a new 'Dyed' that can be then used with 'checkDyed'
dye :: (GHC.Generic a, GFrom a, All (All Top) (GCode a)) => a -> IO (Dyed a)
dye !x = do
  let parts :: [IO Part]
      parts = hcollapse $ hcmap (Proxy @Top) (mapIK go) (gfrom x)
  Dyed <$> sequence parts
  where go :: b -> IO Part
        go !y = Part <$> mkWeakPtr y Nothing

-- | Check if a 'Dyed' is being retained. 
-- Will print to `stderr` if it is being retained.
checkDyed :: Dyed a -> IO ()
checkDyed (Dyed parts) = do
  performGC
  mapM_ checkPart parts
  where 
    checkPart (Part wp) = do
      res <- deRefWeak wp
      case res of
        Just x -> findPtr (unsafeCoerce# x) 1
        Nothing -> pure ()

