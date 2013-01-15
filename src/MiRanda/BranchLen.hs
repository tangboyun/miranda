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

module MiRanda.BranchLen where

import           Control.Arrow
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as H
import           Data.List
import           MiRanda.BranchLen.Newick
import           MiRanda.Parameter.BL
import           MiRanda.Types


isLeaf :: NewickTree a -> Bool
{-# INLINE isLeaf #-}
isLeaf (NTLeaf _ _) = True
isLeaf _ = False


newick :: NewickTree DefDecor
newick = parseNewick treePara


calcBranchLength :: NewickTree DefDecor -> [Label] -> Double
calcBranchLength = flip go
  where
    go :: [Label] -> NewickTree DefDecor -> Double
    go [] _ = 0
    go as tree =
        if null $ intersect as (all_labels tree)
        then 0
        else case tree of
            NTLeaf  (_,d) l ->
                if null $ delete l as
                then d
                else 0
            NTInterior (_,d) ts ->
                let (leafs,subts) = partition isLeaf ts
                    (ld,as') = foldl' (\ac@(acc',rs) (NTLeaf (_,d') l) ->
                                        if l `elem` rs
                                        then (acc'+d',delete l rs)
                                        else ac
                                      ) (0,as) leafs 
                in if null as'
                   then d + ld
                   else d + ld +
                        foldl' (+) 0 (map (go as') subts)

-- | utr branch length and bin idx
toBranchLength :: [Record] -> [(Double,Int)]
toBranchLength records = go H.empty . map ( utr &&& homoUTRs) $ records
  where
    myMedian _ [] = 0
    myMedian l xs | odd l = xs !! ((l-1) `quot` 2)
                  | otherwise = (\(a:b:[]) -> (a+b)/2) $ take 2 $
                                drop ((l `quot` 2) - 1) xs
    at = B8.index
    extract = (show . taxonomyID) &&&
              (unGS . alignment)
    allGapsAt idxs (_,spSeq) =  all (== '-') $
                                map (spSeq `at`) idxs
    go _ [] = []
    go h ((ref,homo):rs) =
        let (refID,refSeq) = extract ref
            is = B8.findIndices (/= '-') refSeq
            n = B8.length refSeq - (B8.count '-' refSeq)
            ts = filter (not . allGapsAt is) .
                 map extract $ homo
            (bs,h') = foldl'
                      (\(acc,ha) idx ->
                        let refN = refSeq `at` idx
                            cs = sort $ map fst $
                                 filter ((== refN) . (`at` idx) . snd) ts
                        in if null cs
                           then (0:acc,ha)
                           else case H.lookup cs ha of
                               Nothing ->
                                   let v = calcBranchLength newick (refID:cs)
                                       ha' = H.insert cs v ha
                                   in (v:acc,ha')
                               Just v -> (v:acc,ha)
                      ) ([],h) is
            m = myMedian n $ sort bs
            i = if m == 0
                then 0
                else (length $ takeWhile (< m) branchLenThresholds) - 1
        in (m,i) : go h' rs
