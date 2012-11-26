{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.Util where

import MiRanda.Types
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.Vector.Unboxed as UV
import Data.ByteString (ByteString)
import Data.List

seedToIdx :: ByteString -> (UV.Vector Int,Int)
seedToIdx miR3' =
  let miR = B8.reverse miR3'
      nS  = B8.length seed
      seed = flip B8.take miR $
             (1 +) $ last $ take 8 $
             B8.findIndices isAlpha miR
      sVec = UV.fromList $ B8.unpack seed
  in (UV.findIndices isAlpha sVec,nS)

lengthOfEach (Align miR3' _ _) =
  let miR = B8.reverse miR3'
      at = UV.unsafeIndex
      idxV = UV.fromList $
             B8.findIndices isAlpha miR
      is = map (idxV `at`) [0,1,7,12,16]
      go [] = []
      go [x] = [B8.drop x miR]
      go (i:j:xs) = (B8.take (j-i) $ B8.drop i miR) : go (j:xs)
  in reverse $ map B8.length $ go is
     
getSeedType :: Align -> SeedType
getSeedType (Align miR3' mR5' b) = 
  let (idxV,nS) = seedToIdx miR3'
      idxV' = UV.fromList [0..7]
      site = UV.fromList $
             B8.unpack $ B8.take nS $
             B8.reverse mR5'
      at = UV.unsafeIndex
      bond = UV.fromList $
             B8.unpack $ B8.take nS $
             B8.reverse b
      noGap = (UV.all isAlpha $ UV.take 8 site) &&
              (idxV == idxV')
      allMatch2_8 = UV.all (== '|') $
                    UV.unsafeBackpermute bond $
                    UV.tail idxV -- if 2~8 mer all match
      allMatch2_7 = UV.all (== '|') $
                    UV.unsafeBackpermute bond $
                    UV.init $ UV.tail $ idxV
      allMatch3_8 = UV.all (== '|') $
                    UV.unsafeBackpermute bond $
                    UV.tail $ UV.tail $ idxV
  in if noGap && allMatch2_8
     then if site `at` UV.head idxV == 'A' -- if site 1 == A
          then M8 -- 8mer
          else M7M8 -- 7mer-m8
     else if noGap && allMatch2_7 -- if 2~7 mer all match
          then if site `at` UV.head idxV == 'A' -- if site 1 == A
               then M7A1 -- 7mer-A1
               else M6 -- 6mer
          else if noGap && allMatch3_8
               then M6O
               else Imperfect


getSeedMatchSite :: MSite -> Pair
getSeedMatchSite site =
  let P _ down = _mRNARange site
      (Align miR3' mR5' b) = _align site
      seed = _seedType site
      (idxV,n) = seedToIdx miR3'
      at = UV.unsafeIndex
      fstC = UV.head idxV
      P i j = case seed of
                M8 -> P 0 7
                M7A1 -> P 0 7
                M7M8 -> P 1 7
                M6   -> P 1 7
                M6O  -> P 2 7
                _    -> P 1 7
  in P (down - idxV `at` j) (down - idxV `at` i)

