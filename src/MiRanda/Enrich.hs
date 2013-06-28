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

module MiRanda.Enrich where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           System.Random.MWC

shuffle :: G.Vector v a => Seed -> v a -> (v a,Seed)
shuffle s v =
  runST $ do
    let len = G.length v
        n   = len-1
    mv <- GM.new len
    gen <- restore s
    G.unsafeCopy mv v
    forM_ [0..n] $ \idx -> do
      idx' <- uniformR (idx,n) gen
      GM.unsafeSwap mv idx idx'
    s' <- save gen
    v' <- G.unsafeFreeze mv
    return $ (v',s')
{-# INLINE shuffle #-}
    
permute :: G.Vector v a => Seed -> Int -> v a -> ([v a],Seed)
permute s n vec =
  let l = G.length vec
      v = G.concat $ replicate n vec
      t = n * l
      end = l - 1
  in runST $ do
    mv <- G.unsafeThaw v
    gen <- restore s
            
    forM_ [0..t-1] $ \idx -> do
      let (c,r) = idx `divMod` l
          i = c * l
      i' <- uniformR (r,end) gen
      GM.unsafeSwap mv idx (i+i') 

    v' <- G.unsafeFreeze mv
    s' <- save gen
    return (map ((\i -> G.unsafeSlice i l v').(*l)) [0..n-1] ,s')
{-# INLINE permute #-}

permEnrich :: (UV.Unbox a,Ord a, Num a) => Seed -> Int -> UV.Vector Int -> UV.Vector a -> (Double,Seed)
permEnrich seed nPerm hitIdxVec geneScoreVec =
    let s = UV.sum $ UV.unsafeBackpermute geneScoreVec hitIdxVec
        nGene = UV.length hitIdxVec
        (vs,seed') = permute seed nPerm $ V.enumFromN 0 (UV.length geneScoreVec)
        ivs = map (V.convert . V.force . V.slice 0 nGene) vs
        n' = length . filter (> s) . map (UV.sum . UV.unsafeBackpermute geneScoreVec) $ ivs
        p = fromIntegral n' / fromIntegral nPerm
    in (p,seed')
