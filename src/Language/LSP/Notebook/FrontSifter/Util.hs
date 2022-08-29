{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}

module Language.LSP.Notebook.FrontSifter.Util (
  binarySearchVec
  ) where

import Data.Bits
import Data.Vector as V hiding (zip)


binarySearchVec :: Vector Int -> Int -> (Int, Bool)
binarySearchVec = binarySearchVec' @Int

{-# SPECIALISE binarySearchVec' :: Vector Int -> Int -> (Int, Bool) #-}
binarySearchVec' :: forall a. (Num a, Eq a, Bits a, Ord a, Integral a) => Vector a -> a -> (a, Bool)
binarySearchVec' [] _ = (0, False)
binarySearchVec' vec desired = binarySearchVec' 0 (fromIntegral $ V.length vec)
  where
    binarySearchVec' :: a -> a -> (a, Bool)
    binarySearchVec' lb ub | lb == ub = if
      | lb < 0 -> (0, False)
      | ub >= fromIntegral (V.length vec) -> (fromIntegral (V.length vec), False)
      | vec ! fromIntegral lb == desired -> (lb, True)
      | otherwise -> (lb, False)
    binarySearchVec' lb ub = case midValue `compare` desired of
      LT -> if mid > lb then binarySearchVec' mid ub else (lb + 1, False)
      EQ -> (mid, True)
      GT -> if mid < ub then binarySearchVec' lb mid else (lb, False)
      where
        mid = shift (lb + ub) (-1)
        midValue = vec ! fromIntegral mid
