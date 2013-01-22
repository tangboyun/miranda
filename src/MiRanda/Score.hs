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
module MiRanda.Score where

import           Control.Applicative
import           Control.Arrow
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as IM
import           Data.List
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           MiRanda.BranchLen
import           MiRanda.BranchLen.Newick
import           MiRanda.Parameter
import           MiRanda.Parameter.BL
import           MiRanda.Parameter.FSPPara
import           MiRanda.Parameter.PCT
import           MiRanda.Types
import           MiRanda.Util
import Text.Printf
import Data.Function

atV :: V.Vector a -> Int -> a
atV = (V.!)

calcPct :: Double -> (Double,Double,Double,Double) -> Double
calcPct bl (b0,b1,b2,b3) = max 0 $! b0 + b1 / ( 1 + exp (negate b2 * bl + b3)) 
    

binTreeVec :: V.Vector (NewickTree DefDecor)
binTreeVec = V.fromList $
             map parseNewick
             [treeBin1
             ,treeBin2
             ,treeBin3
             ,treeBin4
             ,treeBin5
             ,treeBin6
             ,treeBin7
             ,treeBin8
             ,treeBin9
             ,treeBin10]

checkConservation :: SeedType -> Double -> Bool
checkConservation st bl =
    case st of
        M8 -> bl >= 0.8
        M7M8 -> bl >= 1.3
        _ -> bl >= 1.6
{-# INLINE checkConservation #-}
        
getConservations :: [Record] -> [[Conservation]]
{-# INLINE getConservations #-}
getConservations records = go $ zip records (toBranchLength records)
  where
    hashMapLookup st seedStr =
        let h1 = H.fromList pct8mer
            h2 = H.fromList pct7merm8
            h3 = H.fromList pct7mer1a
        in case st of
            M8 -> H.lookup seedStr h1
            M7M8 -> H.lookup seedStr h2
            M7A1 -> H.lookup seedStr h3
            _ -> Nothing
    go [] = []
    go ((r,(_,binIdx)):res) =
        let thisUTR = utr r
            thisID = show . taxonomyID $ thisUTR
            otherUTRs = homoUTRs r
            sites = predictedSites r
            cons = map
                   (\s ->
                     let seed = extractSeedN8 . align $ s
                         sT = seedType s
                         tree = binTreeVec `atV` binIdx
                     in if sT /= M8
                        then let homoIDs = map show $
                                           getSpeciesWithSameSeedSeq s
                                           thisUTR otherUTRs
                                 bl = if null homoIDs
                                      then 0
                                      else calcBranchLength tree (thisID:homoIDs)
                                 isC = checkConservation sT bl
                             in Con isC bl $ fmap (calcPct bl) $
                                hashMapLookup sT seed
                        else let stSps = groupHomoSpWithSeedType s thisUTR otherUTRs
                                 bls = -- trace (show stSps) $
                                       map
                                       (\(stype,ids) ->
                                         -- maybe include current id here ?
                                         let bl = if length ids == 1
                                                  then 0
                                                  else calcBranchLength tree $
                                                       map show ids
                                             isC = checkConservation stype bl
                                         in (stype,bl,isC)
                                       ) stSps
                                 (_,bl,isC) = if null bls
                                              then (M6,0,False)
                                              else foldl1'
                                                   (\a@(_,_,b1) b@(_,_,b2) ->
                                                     if b1
                                                     then a
                                                     else if b2
                                                          then b
                                                          else a) bls
                                 thisPCT = if null bls
                                           then Nothing
                                           else last $ sort $
                                                map (\(stype,thisBL,_) ->
                                                      fmap (calcPct thisBL) $
                                                      hashMapLookup stype seed
                                                    ) bls
                             in Con isC bl thisPCT) sites
        in cons : go res


(!) :: IM.IntMap a -> IM.Key -> a
(!) = (IM.!)

getRawScore :: UTR -> Int -> MSite -> RawScore
getRawScore utr l s =
  let posScore = getPosScore (getSeedMatchSite s) l
      paScore = getPairScore (_seedType s) (_align s)
      utrSD = B8.filter isAlpha $! unGS $ alignment utr
      auScore = getAUScore utrSD s
  in RS paScore auScore posScore
{-# INLINE getRawScore #-}

getPosScore :: Pair -> Int -> PosScore
getPosScore (P a b) utrL = PosScore $! min maxDistToNearestEndOfUTREnd $!
                           min (fromIntegral a) (fromIntegral $! utrL-1-b)
  where
    maxDistToNearestEndOfUTREnd = 1500
    
getPairScore :: SeedType -> Align -> PairScore
{-# INLINE getPosScore #-}
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
    -- baseScore c = if c == '|'  
    --               then 0.5
    --               else if c == ':'
    --                    then 0.25
    --                    else 0
    baseScore c = if c == '|' -- use same scoring schme as targetscan 6.2
                  then 0.5
                  else 0
    scanScore idx score preScore@(maxScore,(miIdx,utrIdx))
      | idx < n   =
        if B8.index bond idx /= ' ' -- here, targetscan 6.2 seems not stop at GU
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

getAUScoreImpl :: SeedType -> Pair -> ByteString -> AUScore
{-# INLINE getAUScoreImpl #-}
getAUScoreImpl st (P up' dn) utr =
  let up = case st of
          M8 -> up' - 1
          M7M8 -> up' - 1
          M6O -> up' - 1
          _ -> up' - 2

      (us,ds) = let ls = map (1/) [2.0..]
                    ls1 = 1:ls
                    ls2 = 0.5:ls
                in case st of
                  M8 -> (ls1,ls)
                  M7M8 -> (ls1,ls2)
                  M7A1 -> (ls,ls)
                  _   -> (ls,ls2)
      up30 = B8.unpack $ B8.take 30 $
             B8.map ((\c ->
                       if c == 'T'
                       then 'U'
                       else c) . toUpper) $
             B8.reverse $ B8.take up utr
      dn30 = B8.unpack $ B8.take 30 $
             B8.map ((\c ->
                       if c == 'T'
                       then 'U'
                       else c) . toUpper) $
             B8.drop dn utr
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
  in AUScore $! local / total

getAUScore :: ByteString -> MSite -> AUScore
getAUScore utr site =
    let p = getSeedMatchSite site
        st = _seedType site
    in getAUScoreImpl st p utr
{-# INLINE getAUScore #-}

getSiteContrib :: SeedType -> Maybe Double
getSiteContrib st = siteContribMap ! fromEnum st
{-# INLINE getSiteContrib #-}

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
{-# INLINE getContextScore #-}

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
{-# INLINE getSPSTA #-}         

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
{-# INLINE getContextScorePlus #-}

mergeScore :: (Record,[Conservation]) -> RefLine
mergeScore (r,cs) =
    let ss = predictedSites r
        totalM = foldl1' add $ map miRandaScore ss
        totalCon = foldl1' add cs
        totalR = foldl1' add $ map rawScore ss
        totalCS = foldl1' add $ map contextScore ss
        totalCSP = foldl1' add $ map contextScorePlus ss
        totalS = length ss
        (conSite,nonConSite) =
            foldl'
            (\(con@(a1,b1,c1,d1,e1,f1),nonCon@(a2,b2,c2,d2,e2,f2)) (s,c) ->
              if isConserved c
              then let con' = case seedType s of
                           M8 -> (a1+1,b1,c1,d1,e1,f1)
                           M7M8 -> (a1,b1+1,c1,d1,e1,f1)
                           M7A1 -> (a1,b1,c1+1,d1,e1,f1)
                           M6 -> (a1,b1,c1,d1+1,e1,f1)
                           M6O -> (a1,b1,c1,d1,e1+1,f1)
                           Imperfect -> (a1,b1,c1,d1,e1,f1+1)
                   in (con',nonCon)
              else let nonCon' = case seedType s of
                           M8 -> (a2+1,b2,c2,d2,e2,f2)
                           M7M8 -> (a2,b2+1,c2,d2,e2,f2)
                           M7A1 -> (a2,b2,c2+1,d2,e2,f2)
                           M6 -> (a2,b2,c2,d2+1,e2,f2)
                           M6O -> (a2,b2,c2,d2,e2+1,f2)
                           Imperfect -> (a2,b2,c2,d2,e2,f2+1)
                   in (con,nonCon')
            ) ((0,0,0,0,0,0),(0,0,0,0,0,0)) $ zip ss cs
        ut = utr r
        u = B8.filter isAlpha . extractSeq . utr $ r
        ul = B8.length u
        g = Gene (geneSymbol ut) (refSeqID ut)
    in RL (miRNA r) g totalM totalCon totalR totalCS
       totalCSP totalS conSite nonConSite ul u
{-# INLINE mergeScore #-}        

getSites :: [(Record,[Conservation])] -> [(Record,[SiteLine])]
getSites [] = []
getSites ((r,cons):rs) =
    let ss = predictedSites r
        mi = miRNA r
        u = utr r
        g = Gene (geneSymbol u) (refSeqID u)
        sls = map (\(con,s) ->
                    let raw = rawScore s
                        mS = miRandaScore s
                        conS = contextScore s
                        conSP = contextScorePlus s
                        seedM = seedMatchRange s
                        siteM = utrRange s
                        st = seedType s
                        al = align s
                    in SL mi g mS con raw conS conSP seedM siteM st al
                  ) $ zip cons ss
    in (r,sls): getSites rs
