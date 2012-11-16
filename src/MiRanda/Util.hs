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
      go [x] = B8.drop x miR
      go (i:j:xs) = B8.take (j-i) $ B8.drop i miR
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

getPairScore :: SeedType -> Align -> PairScore
getPairScore s (Align miR3' mR5' b) =
  PS $
  scanScore len 0
  (0,(getIdx len miRNA,
      getIdx len utr))
  where
    getIdx l poly = (length $
                     B8.findIndices isAlpha $
                     B8.take l poly) - 1
    idxV = UV.fromList $
           B8.findIndices isAlpha $
           B8.reverse miR3'
    n = B8.length b
    at = UV.unsafeIndex
    idx7 = idxV `at` 6
    idx8 = idxV `at` 7
    len = case s of
            M6 -> idx7
            M7A1 -> idx7
            _    -> idx8
    miRNA = B8.reverse miR3'
    utr   = B8.reverse mR5'
    idxFor13 = idxV `at` 12
    idxFor16 = idxV `at` 15
    bond = B8.reverse b
    baseScore c = if c == '|'
                  then 0.5
                  else if c == ':'
                       then 0.25
                       else 0
    scanScore idx score preScore@(maxScore,(miIdx,utrIdx))
      | idx < n   =
        if B8.index bond idx /= ' '
        then if idxFor13 <= idx && idx <= idxFor16
             then let score' = score + 2*baseScore (B8.index bond idx)
                  in scanScore (idx+1) score' preScore
             else let score' = score + baseScore (B8.index bond idx)
                  in scanScore (idx+1) score' preScore
        else if score >= maxScore
             then let miIdx' = getIdx (idx-1) miRNA
                      utrIdx' = getIdx (idx-1) utr
                  in scanScore (idx+1) 0 (score,(miIdx',utrIdx'))
             else scanScore (idx+1) 0 preScore
      | otherwise = let offset = abs $ miIdx - utrIdx
                    in maxScore - max 0 (0.5 * fromIntegral (offset - 2))

getSeedMatchSite :: Site -> Pair
getSeedMatchSite site =
  let P _ down = mRNARange site
      (Align miR3' mR5' b) = align site
      seed = seedType site
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

  
getAUScore :: ByteString -> Site -> AUScore
getAUScore utr site =
  let P up dn = getSeedMatchSite site
      (us,ds) = let ls = map (1/) [2.0..]
                    ls1 = 1:ls
                    ls2 = 0.5:ls
                in case seedType site of
                  M8 -> (ls1,ls)
                  M7M8 -> (ls1,ls2)
                  M7A1 -> (ls,ls)
                  _   -> (ls,ls2)
                  
      up30 = B8.unpack $ B8.take 30 $
             B8.map ((\c ->
                       if c == 'U'
                       then 'T'
                       else c) . toUpper) $
             B8.reverse $ B8.take up utr
      dn30 = B8.unpack $ B8.take 30 $
             B8.map ((\c ->
                       if c == 'U'
                       then 'T'
                       else c) . toUpper) $
             B8.drop (dn+1) utr
      total = foldl1' (+) $
              zipWith (\_ b -> b) up30 us ++
              zipWith (\_ b -> b) dn30 ds
      local = foldl1' (+) $
              zipWith (\c s ->
                        if c == 'A' || c == 'U'
                        then s
                        else 0) up30 us ++
              zipWith (\c s ->
                        if c == 'A' || c == 'U'
                        then s
                        else 0) dn30 ds
  in AU $ local / total
   
