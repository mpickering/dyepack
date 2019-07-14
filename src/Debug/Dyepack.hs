{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE TypeApplications, FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
module Debug.Dyepack (mkDyed, checkDyed, Dyed) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Generics.SOP.GGP
import GHC.Exts
import System.Mem.Weak
import System.Mem

foreign import ccall unsafe "findPtr" findPtr :: Ptr a -> Int -> IO ()

-- TODO: come up with a better name for Part
data Part = forall a. Part (Weak a)
newtype Dyed a = Dyed [Part]

mkDyed :: (GHC.Generic a, GFrom a, All (All Top) (GCode a)) => a -> IO (Dyed a)
mkDyed x = do
  let parts :: [IO Part]
      parts = hcfoldMap (Proxy @Top) go (gfrom x)
  Dyed <$> sequence parts
  where go :: I a1 -> [IO Part]
        go (I y) = [Part <$> mkWeakPtr y Nothing]

checkDyed :: Dyed a -> IO ()
checkDyed (Dyed parts) = do
  performGC
  mapM_ checkPart parts
  where 
    checkPart (Part wp) = do
      res <- deRefWeak wp
      case res of
        Just x -> putStrLn "leak" >> findPtr (unsafeCoerce# x) 1
        Nothing -> putStrLn "released"

