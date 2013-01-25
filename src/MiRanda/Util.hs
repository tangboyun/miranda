{-# LANGUAGE OverloadedStrings, BangPatterns #-}
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
import Control.Arrow
import Data.Function
import Data.Maybe
import Data.Monoid

getGapRangeFromTrueRange :: Pair -> UTR -> (Int,Int)
getGapRangeFromTrueRange !p !utr =
    let !seqStr = extractSeq utr
        !charCount = UV.postscanl (+) 0 $ UV.fromList $
                     map (\c ->
                           if c == '-'
                           then 0 :: Int
                           else 1) $
                     B8.unpack $ seqStr
        !rbeg = case UV.findIndex (== (1+beg p)) charCount of
            Nothing -> error $ "getGapRangeFromTrueRange: beg findIdx =" ++ show (1+beg p) ++ "\n" ++
                       show p
            Just x -> x
            
        !rend = case UV.findIndex (== (end p)) charCount of
            Nothing -> error $ "getGapRangeFromTrueRange: end findIdx=" ++ show (end p)
            Just x -> x
        !rend' = rend + 1
    in (rbeg,rend')
{-# INLINE getGapRangeFromTrueRange #-}

-- | 仅返回homos中的taxID,当seedType为8mer以外时,调用该函数决定BL
getSpeciesWithSameSeedSeq :: Site -> UTR -> [UTR] -> [Int]
getSpeciesWithSameSeedSeq !s !utr homos =
    let !seedR = seedMatchRange s
        !seqStr = extractSeq utr
        !sPair = getGapRangeFromTrueRange seedR utr
        !seed = extractStr sPair seqStr
    in map snd $ filter ((== seed) . fst) $
       map ((extractStr sPair . extractSeq) &&& taxonomyID) homos
{-# INLINE getSpeciesWithSameSeedSeq #-}
       
-- | 返回各种类型Seed对应的序列相同的物种分组
groupHomoSpWithSeedType :: Site -> UTR -> [UTR] -> [(SeedType,[Int])]
{-# INLINE groupHomoSpWithSeedType #-}
groupHomoSpWithSeedType s utr homos =
    let seedR = seedMatchRange s
        sPair = getGapRangeFromTrueRange seedR utr
        miRStr = tToU . miRNASite3' . align $ s
    in  sortBy (compare `on` fst) $ map ((fst . head) &&& (map snd)) $
        map (map snd .
             maximumBy (compare `on` length) .
             groupBy ((==) `on` fst)) $
        groupBy ((==) `on` (fst . snd)) $
        sortBy
        (\ a b ->
          compare  (fst . snd $ a) (fst . snd $ b) <>
          compare  (fst a) (fst b)
        ) $
        map
        (\(siteStr,taxID) ->
          let b = B8.reverse $ B8.pack $
                  B8.zipWith
                  (\a b ->
                    if (a == 'A' && b == 'U') ||
                       (a == 'U' && b == 'A') ||
                       (a == 'G' && b == 'C') ||
                       (a == 'C' && b == 'G')
                    then '|'
                    else if (a == 'G' && b == 'U') ||
                            (a == 'U' && b == 'G')
                         then ':'
                         else ' ')
                  -- 必需从反向开始检测匹配,
                  -- 因为miRStr 和 siteStr因为插入和删除可能不等长
                  -- 源于miranda的site位点信息不准确，其site位点区间包含'-'
                  -- 这里只需要检测seed match类型，从反向开始逆推"大概应该"是安全的
                  -- TargetScan的位点检测也不准确，它是以3’配对最大化来决定热力学结构的
                  -- 由热力学结构来判定3'配对应该更科学些
                  (B8.reverse miRStr) (B8.reverse siteStr)
              al = Align miRStr siteStr b
              seedT = getSeedType al
              sMStr = getSiteSeqAtSeedMatch al
              
          in (sMStr,(seedT,taxID))) $
        map ((tToU . extractStr sPair . extractSeq) &&& taxonomyID) $ utr:homos

        
tToU :: ByteString -> ByteString
tToU = B8.map ((\c -> if c == 'T'
                      then 'U'
                      else c) . toUpper)
{-# INLINE tToU #-}       
        
extractStr :: (Int,Int) -> ByteString -> ByteString
extractStr (!i,!j) = B8.take (j-i) . B8.drop i
{-# INLINE extractStr #-}

extractSeq :: UTR -> ByteString
extractSeq = unGS . alignment
{-# INLINE extractSeq #-}

extractSeedN8FromBS :: ByteString -> ByteString
{-# INLINE extractSeedN8FromBS #-}
extractSeedN8FromBS miR =
    let !mi = B8.reverse miR
    in B8.pack $ map (B8.index mi) $
       take 7 $ drop 1 $ B8.findIndices isAlpha mi

extractSeedN8 :: Align -> ByteString
{-# INLINE extractSeedN8 #-}
extractSeedN8 (Align miR _ _) = extractSeedN8FromBS miR


seedToIdx :: ByteString -> (UV.Vector Int,Int)
{-# INLINE seedToIdx #-}
seedToIdx miR3' =
  let !miR = B8.reverse miR3'
      !nS  = B8.length seed
      !seed = flip B8.take miR $
              (1 +) $ last $ take 8 $
              B8.findIndices isAlpha miR
      !sVec = UV.fromList $ B8.unpack seed
  in (UV.findIndices isAlpha sVec,nS)


lengthOfEach (Align !miR3' _ _) =
  let miR = B8.reverse miR3'
      at = UV.unsafeIndex
      idxV = UV.fromList $
             B8.findIndices isAlpha miR
      is = map (idxV `at`) [0,1,7,12,16]
      go [] = []
      go [x] = [B8.drop x miR]
      go (i:j:xs) = (B8.take (j-i) $ B8.drop i miR) : go (j:xs)
  in reverse $ map B8.length $ go is
{-# INLINE lengthOfEach #-}

getSeedType :: Align -> SeedType
{-# INLINE getSeedType #-}
getSeedType (Align !miR3' !mR5' !b) = 
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
     then if toUpper (site `at` UV.head idxV) == 'A' -- if site 1 == A
          then M8 -- 8mer
          else M7M8 -- 7mer-m8
     else if noGap && allMatch2_7 -- if 2~7 mer all match
          then if toUpper (site `at` UV.head idxV) == 'A' -- if site 1 == A
               then M7A1 -- 7mer-A1
               else M6 -- 6mer
          else if noGap && allMatch3_8
               then M6O
               else Imperfect
                    
-- 返回结合位点
getSeedMatchSite :: MSite -> Pair
{-# INLINE getSeedMatchSite #-}
getSeedMatchSite !site =
  let P _ !down = _mRNARange site
      (Align !miR3' !mR5' !b) = _align site
      !seed = _seedType site
      (!idxV,!n) = seedToIdx miR3'
      at = UV.unsafeIndex
      (i,j) = case seed of
                M8 -> (0, 7)
                M7M8 -> (0, 7)
                M7A1 -> (0, 6)
                M6O -> (2, 7)
                _    -> (1, 6)
      num = 1 + idxV `at` j
  in P (down - num) (down - idxV `at` i)


getSiteSeqAtSeedMatch :: Align -> ByteString
{-# INLINE getSiteSeqAtSeedMatch #-}
getSiteSeqAtSeedMatch al@(Align !miR3' !mR5' !b) =
  let (idxV,n) = seedToIdx miR3'
      !down = B8.length mR5'
      at = UV.unsafeIndex
      (i,j) = case getSeedType al of
                M8 -> (0, 7)
                M7M8 -> (0, 7) -- 7mer-m8 和 07年 paper上的不一样
                M7A1 -> (0, 6)
                M6O -> (2, 7)
                _    -> (1, 6)
      num = 1 + idxV `at` j
  in extractStr ((down - num),(down - idxV `at` i)) mR5'
