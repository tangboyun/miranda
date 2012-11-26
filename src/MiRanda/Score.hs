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
module MiRanda.Score where

import MiRanda.Types
import qualified Data.IntMap.Strict as IM
import Control.Applicative
import Data.List
import qualified Data.Vector.Unboxed as UV
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Char
import Data.List
import MiRanda.Parameter.FSPPara
import MiRanda.Parameter
import MiRanda.Util

(!) :: IM.IntMap a -> IM.Key -> a
(!) = (IM.!)

getRawScore :: UTR -> Int -> MSite -> RawScore
getRawScore utr l s =
  let posScore = getPosScore (_mRNARange s) l
      paScore = getPairScore (_seedType s) (_align s)
      utrSD = B8.filter isAlpha $ unGS $ alignment utr
      auScore = getAUScore utrSD s
  in RS paScore auScore posScore

getPosScore :: Pair -> Int -> PosScore
getPosScore (P a b) utrL = PosScore $ min (fromIntegral a) (fromIntegral $ utrL-1-b)

getPairScore :: SeedType -> Align -> PairScore
getPairScore s (Align miR3' mR5' b) =
  PairScore $
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
  
getAUScore :: ByteString -> MSite -> AUScore
getAUScore utr site =
  let P up dn = getSeedMatchSite site
      (us,ds) = let ls = map (1/) [2.0..]
                    ls1 = 1:ls
                    ls2 = 0.5:ls
                in case _seedType site of
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
  in AUScore $ local / total

getSiteContrib :: SeedType -> Maybe Double
getSiteContrib st = siteContribMap ! fromEnum st

getContextScore :: SeedType -> RawScore -> Maybe ContextScore
getContextScore st (RS (PairScore pairS) (AUScore auS) (PosScore posS)) =
  let siteContrib = getSiteContrib st
      paCoef = paCoefMap ! fromEnum st
      auCoef = auCoefMap ! fromEnum st
      psCoef = psCoefMap ! fromEnum st
      paContrib = toScore <$> pure pairS <*> siteContrib <*> paCoef
      auContrib = toScore <$> pure auS <*> siteContrib <*> auCoef
      psContrib = toScore <$> pure posS <*> siteContrib <*> psCoef
      context = foldl1' (\a b -> (+) <$> a <*> b) [siteContrib,paContrib,auContrib,psContrib]
  in CS <$> context <*> paContrib <*> auContrib <*> psContrib <*> siteContrib
  where
    toScore :: Double -> Double -> Coef -> Double
    toScore score mean (Coef sl inter) = score * sl + inter - mean


getSPSTA :: SeedType -> ByteString -> (Maybe SPScore, Maybe TAScore)
getSPSTA st seedWithN8 =
  let idx = pack seedWithN8
      (a,b,c) = toFSPPara idx
  in case st of
    M8 -> (Just (SPScore a), Just (TAScore c))
    M7M8 -> (Just (SPScore a), Just (TAScore c))
    M7A1 -> (Just (SPScore b), Just (TAScore c))
    M6 -> (fmap SPScore $ IM.lookup idx spsMapmer6, Just (TAScore c))
    M6O -> (fmap SPScore $ IM.lookup idx spsMapmer6, Just (TAScore c))
    _   -> (Nothing, Just (TAScore c))
  where
    cToI :: Char -> Int
    cToI c =
      case toUpper c of
        'A' -> 0
        'C' -> 1
        'G' -> 2
        'T' -> 3
        'U' -> 3
        _   -> error "Invalid char in cToI"
    pack :: ByteString -> Int
    pack bs =
      let str = B8.unpack $ B8.reverse bs
      in sum $ map (\(i,c) -> cToI c * 4^i) $ zip [0..] str

getContextScorePlus :: SeedType -> Align -> RawScore -> Maybe ContextScorePlus
getContextScorePlus st ali rawScore =
  let seedWithN8 = extractSeedN8 ali
      (spsScore,taScore) = getSPSTA st seedWithN8
      (PairScore paS) = pairingScore rawScore
      (AUScore auS) = auScore rawScore
      (PosScore psS) = posScore rawScore
      spsS = case spsScore of
               Nothing -> Nothing
               Just (SPScore s) -> Just s
      taS = case taScore of
              Nothing -> Nothing
              Just (TAScore t) -> Just t
      paS' = fun <$> pure paS <*>
             paMinMaxMap ! fromEnum st <*>
             paRegMeanMap ! fromEnum st
      auS' = fun <$> pure auS <*>
             auMinMaxMap ! fromEnum st <*>
             auRegMeanMap ! fromEnum st
      psS' = fun <$> pure psS <*>
             psMinMaxMap ! fromEnum st <*>
             psRegMeanMap ! fromEnum st
      spsS' = fun <$> spsS <*>
              spsMinMaxMap ! fromEnum st <*>
              spsRegMeanMap ! fromEnum st
      taS' = fun <$> taS <*>
             taMinMaxMap ! fromEnum st <*>
             taRegMeanMap ! fromEnum st
      fcS = fcMap ! fromEnum st
      csp = foldl1' (\a b -> (+) <$> a <*> b)
            [fcS,paS',auS',psS',spsS',taS']
  in CSP <$> csp <*> paS' <*> auS' <*> psS' <*>
             taS' <*> spsS' <*> fcS
  where
    fun s (minV,maxV) (reg,mean) =
      let s' = (s - minV) / (maxV - minV)
      in reg * (s' - mean)
    extractSeedN8 (Align miR _ _) = B8.take 7 $
                                    B8.drop 1 $ B8.reverse miR
