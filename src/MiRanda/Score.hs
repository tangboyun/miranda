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

(!) :: IM.IntMap a -> IM.Key -> a
(!) = (IM.!)
  
getSiteContrib :: SeedType -> Maybe Double
getSiteContrib st = siteContribMap ! fromEnum st

getContextScore :: SeedType -> PairScore -> AUScore -> PosScore -> Maybe ContextScore
getContextScore st (PairScore pairS) (AUScore auS) (PosScore posS) =
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

getFSPara :: ByteString -> TSP
getFSPara bs =
  let (a,b,c) = toFSPPara $ pack bs
  in TSP a b c
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
