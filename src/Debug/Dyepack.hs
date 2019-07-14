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

-- TODO: come up with better names
-- get rid of Something
data Something = forall a. Something a
data Part = forall a. Part (Weak a)
newtype Dyed a = Dyed [Part]

mkDyed :: (GHC.Generic a, GFrom a, All (All Top) (GCode a)) => a -> IO (Dyed a)
mkDyed x = do
  let parts :: [Something]
      parts = hcfoldMap (Proxy @Top) go (gfrom x)
  Dyed . map Part <$> mapM (`mkWeakPtr` (Just (putStrLn "gc"))) parts
  where go :: I a1 -> [Something]
        go (I y) = [Something y]

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
