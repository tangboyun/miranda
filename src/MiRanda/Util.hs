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

seedToIdx :: ByteString -> (UV.Vector Int,Int)
seedToIdx miR3' =
  let miR = B8.reverse miR3'
      nS  = B8.length seed
      seed = flip B8.take miR $
             (1 +) $ last $ take 8 $
             B8.findIndices isAlpha miR
      sVec = UV.fromList $ B8.unpack seed
  in (UV.findIndices isAlpha sVec,nS)

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

getPairScore :: SeedType -> Align -> PairScore
getPairScore s (Align miR3' mR5' b) = scanScore len (0,(0,0))
  where
  (idxV,_) = seedToIdx miR3'
  at = UV.unsafeIndex
  idx7 = idxV `at` 6
  idx8 = idxV `at` 7
  len = case s of
          M6 -> idx7
          M7A1 -> idx7
          _    -> idx8
  idx13 = idxV `at` 12
  idx16 = idxV `at` 15
  bond = UV.drop len $
         UV.fromList $
         B8.unpack $ B8.reverse b
  
