-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.CeRNA where

import MiRanda.Storage.Type
import MiRanda.Types (UTR(..),GapSeq(..),Pair(..),ContextScorePlus(..))
import qualified Data.ByteString.Char8 as B8
import Data.Char (isAlpha)
import qualified Data.HashMap.Strict as H
import Control.Arrow
import Data.Function
import Control.Applicative
import Data.List
import Data.ByteString (ByteString)
import qualified Data.Vector.Unboxed as UV
import Data.Maybe

mutameScore :: Int -- ^ UTR length
            -> Int -- ^ number of miRNAs considered
            -> [[(Int,Int)]] -- ^ sites for microRNAs predicted to target
            -> Double
{-# INLINE mutameScore #-}
mutameScore len nmiR bss =
  if null bss'
  then 0
  else c1 * -- shared miRNAs / total miRNAs
       c2 * -- sum for all miRNA,
            -- number of sites / distance span
            -- for 1 site
            -- 1 / min (beg from 5',end from 3')
       c3 * -- sum for all miRNA,
            -- (distance spaned)^2 / sum of the squared distances between
            -- successive MREs
       c4   -- (#MREs in X for all considered microRNAs - #microRNAs predicted to target X + 1) /
            -- #MREs in X for all considered microRNAs
  where
    bss' = map sort $ filter (not . null) bss
    nRNA = length bss'
    totalN = sum nForEach
    nForEach = map (fromIntegral . length) bss'
    c1 = (fromIntegral nRNA) / fromIntegral nmiR 
    c2 = sum $ zipWith (/) nForEach $ map f1 bss'
    c3 = sum $ zipWith (/) (map ((^^2) . f1) bss') (map f2 bss')  -- unclear 
    c4 = (totalN - (fromIntegral nRNA) + 1) / totalN
    f1 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in max (leng-l) l
    f1 xs = let (a1,b1) = head xs
                (a2,b2) = last xs
            in (fromIntegral (a2+b2)) / 2 - 
               (fromIntegral (a1+b1)) / 2
    f2 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in l ^^ 2 + (leng - l) ^^ 2
    f2 (x:xs) = fst $
                foldl' (\(acc,(a1,b1)) (a2,b2) ->
                         let l1 = fromIntegral (a1+b1) / 2
                             l2 = fromIntegral (a2+b2) / 2
                             acc' = acc + (l2 - l1) ^^ 2
                         in (acc',(a2,b2))
                       ) (0,x) xs


ceRNAScore :: GeneRecord -> GeneRecord -> Double
{-# INLINE ceRNAScore #-}
ceRNAScore (GR _ mirSs1) (GR (GI _ _ uStr _) mirSs2) =
    let l = B8.length $ B8.filter isAlpha $
            unGS $ alignment uStr
        total = length mirSs1
        ss = map snd $ intersection mirSs2 mirSs1
    in mutameScore l total ss

intersection :: [MiRSites] -> [MiRSites] -> [(ByteString,[(Int,Int)])]
intersection = (H.toList .) .
               (H.intersection `on`
                (H.fromList . map
                 ((identity . mir) &&&
                  (map ((beg &&& end) . siteRange) . sites)
                 )))

sharePercentageGE :: Double -> GeneRecord -> GeneRecord -> Bool
{-# INLINE sharePercentageGE #-}
sharePercentageGE p gr1 gr2 =
    let i = length $ (intersection `on` mirSites) gr2 gr1
        n = length $ mirSites gr1
    in ((/) `on` fromIntegral) i n >= p


toLine :: GeneRecord -> GeneRecord
       -> (Double,(UV.Vector Int,UV.Vector Double))
toLine gr1 gr2 =
    let h1 = (H.intersection `on`
              (H.fromList . map
               ((identity . mir) &&&
                (map ((beg &&& end) . siteRange) . sites)
               ) . mirSites)) gr2 gr1
        h2 = (H.intersection `on`
              (H.fromList .
               map
               ((identity . mir) &&&
                (foldl1' (+) . map (fromMaybe 0 . fmap contextPlus) . map contextScorePlus . sites)
               ) . mirSites)) gr2 gr1             
        vi = UV.fromList $
             map
             (fromMaybe 0 . fmap length .
              (`H.lookup` h1) . identity . mir) $
             mirSites gr1
        vd = UV.fromList $
             map
             (fromMaybe 0 .
              (`H.lookup` h2) . identity . mir) $
             mirSites gr1
    in (ceRNAScore gr1 gr2,(vi,vd))
