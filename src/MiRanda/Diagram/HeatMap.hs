{-# LANGUAGE FlexibleContexts, OverloadedStrings, BangPatterns #-}
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

module MiRanda.Diagram.HeatMap
       ( plotCeRNAHeatmap
--       , mkDataset
       ) 
           where

import MiRanda.Storage.Type
import MiRanda.CeRNA
import MiRanda.Types (Gene(..),ContextScorePlus(..))
import Diagrams.HeatMap
import Diagrams.HeatMap.Type
import MiRanda.Storage.Type
import Data.Text.Encoding
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Arrow
import Control.Applicative
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Statistics.Function
import Diagrams.Prelude hiding ((<>))
import Diagrams.TwoD.Text
import Diagrams.TwoD.Image
import qualified Data.Vector.Generic as GV
import Statistics.Sample
import Statistics.Quantile
import Data.Monoid
import Control.Monad.ST
import System.Random.MWC
import Data.List
import Control.Parallel.Strategies
import qualified Data.Vector.Fusion.Stream as SF
import qualified Data.HashMap.Strict as H
import qualified Data.Set as S

plotCeRNAHeatmap :: (Renderable (Path R2) b, Renderable Text b, Renderable Image b, Backend b R2)
                 => GeneRecord -> [GeneRecord] -> (Diagram b R2,Diagram b R2)
plotCeRNAHeatmap gr grs =
    let colorOpt1 = Two white blue
        colorOpt2 = Two blue white
        clustOpt1 = ClustOpt colorOpt1
                    (Just (euclidAndCorr,CompleteLinkage,LeftTree))
                    (Just (euclidAndCorr,CompleteLinkage,TopTree))
        clustOpt2 = ClustOpt colorOpt2
                    (Just (euclidAndCorr,CompleteLinkage,LeftTree))
                    (Just (euclidAndCorr,CompleteLinkage,TopTree))
        mH = fromIntegral (nRow $ datM dset1) * 12
        mW = fromIntegral (nCol $ datM dset1) * 12
        treeH = (min mH mW) * 0.25
        lineW = (min mH mW) * 5.0e-5
        fontS = 10
        fontN = "Arial"
        dp@(dset1,dset2) = mkDataset gr grs
        toColorV = (\(i,a) ->
                     ColorVal i ((i+a)*0.5) a
                   ) . minMax . dat . datM
        -- toColorV2 = (\v ->
        --               let i = continuousBy medianUnbiased 2 100 v
        --                   a = UV.maximum v
        --               in ColorVal i ((i+a)*0.5) a
        --             ) . dat . datM
        colorV1 = toColorV dset1
        colorV2 = toColorV dset2
        para1 = Para clustOpt1 colorV1 mH mW treeH lineW treeH
                lineW fontS fontS fontS fontN Horizontal Quality
        para2 = para1 { clustOpt = clustOpt2
                      , colorVal = colorV2 }
    in (fst . plotHeatMap para1) ***
       (fst . plotHeatMap para2) $ dp


-- plotCeRNASite :: (Renderable (Path R2) b, Renderable Text b, Renderable Image b, Backend b R2)
--                  => Dataset -> Diagram b R2
-- plotCeRNASite dset =
--     let colorOpt1 = Two white blue
--         clustOpt1 = ClustOpt colorOpt1 
--                     (Just (euclideanDistance,CompleteLinkage,LeftTree))
--                     (Just (euclideanDistance,CompleteLinkage,TopTree))
--         mH = fromIntegral (nRow $ datM dset) * 12
--         mW = fromIntegral (nCol $ datM dset) * 12
--         treeH = (min mH mW) * 0.25
--         lineW = (min mH mW) * 5.0e-5
--         fontS = 10
--         fontN = "Arial"
--         toColorV = (\(i,a) ->
--                      ColorVal i ((i+a)*0.5) a
--                    ) . minMax . dat . datM
--         colorV1 = toColorV dset
--         para1 = Para clustOpt1 colorV1 mH mW treeH lineW treeH
--                 lineW fontS fontS fontS fontN Horizontal Quality
--     in fst $ plotHeatMap para1 dset

-- mkSite :: GeneRecord -> [GeneRecord] -> Dataset
-- mkSite gr grs =
--     let colNs = V.fromList $ map (decodeUtf8 . identity . mir) $ mirSites gr
--         miSs = map (accession . mir) $ mirSites gr
--         getRowName r = (\g ->
--                          let n1 = decodeUtf8 $ ref g
--                              n2 = decodeUtf8 $ ref g <> "(" <> syb g <> ")" 
--                          in if B8.null $ syb g
--                          then n1
--                          else n2
--                        ) $ gene $ geneInfo r
--         mkD m1 = Dataset Nothing Nothing
--                  Nothing m1
--         mkM v = let rowNum = UV.length v `div` colNum
--                     colNum = V.length colNs
--                 in Matrix rowNum colNum RowMajor v
--     in mkD $
--        mkM $
--        (UV.fromList $ map (fromIntegral . length . sites) $ mirSites gr) UV.++
--        getSiteNum UV.empty miSs grs
--   where
--     getSiteNum !accV _ [] = accV
--     getSiteNum !accV ss (r:rs) =
--         let set = S.fromList ss
--             h = H.fromList $
--                  map ((accession . mir) &&& (fromIntegral . length . sites)) $
--                  filter ((`S.member` set) . accession . mir) $ mirSites r
--             s = UV.fromList $ map (fromMaybe 0 . (`H.lookup` h)) ss
--             !accV' = accV UV.++ s
--         in getSiteNum accV' ss rs
            
mkDataset :: GeneRecord -> [GeneRecord] -> (Dataset,Dataset)
mkDataset gr grs =
    let colNs = V.fromList $ map (decodeUtf8 . B8.copy . identity . mir) $ mirSites gr
        getRowName r = (\g ->
                         let n1 = decodeUtf8 $ ref g
                             n2 = decodeUtf8 $ ref g <> "(" <> syb g <> ")" 
                         in if B8.null $ syb g
                         then n1
                         else n2
                       ) $ gene $ geneInfo r
        fstP = ((UV.fromList . map (fromIntegral . length . sites)) &&&
                (UV.fromList .
                 map (sum . map (fromMaybe 0 . fmap contextPlus .
                                 contextScorePlus) . sites))
                ) $ mirSites gr
        mkD (rowNs,(m1,m2)) = ( Dataset
                                (Just rowNs)
                                (Just colNs)
                                Nothing m1
                              , Dataset
                                (Just rowNs)
                                (Just colNs)
                                Nothing m2)
        mkM v =
            let rowNum = UV.length v `div` colNum
                colNum = V.length colNs
            in Matrix rowNum colNum RowMajor v
    in mkD $
       second ((mkM . (fst fstP <>)) *** (mkM . (snd fstP <>))) $
       first (V.cons (getRowName gr)) $
       foldl'
       (\(rVec,(vi,vd)) (rN,(v1,v2)) ->
         let rVec' = rVec `V.snoc` rN
             vi' = vi <> (UV.map fromIntegral v1)
             vd' = vd <> v2
         in (rVec',(vi',vd'))) (V.empty,(UV.empty,UV.empty)) $
       map (getRowName &&& (snd . toLine gr)) grs


pcc,corr,euclideanDistance,euclidAndCorr :: UV.Vector Double -> UV.Vector Double -> Double
pcc v1 v2 =
  let m1 = mean v1
      m2 = mean v2
  in (\(num,(res1,res2)) ->
       num / (sqrt $ res1 * res2)
       ) $
     UV.ifoldl'
     (\(num,(res1,res2)) i e1 ->
       let val1 = e1
           val2 = UV.unsafeIndex v2 i
           num' = num + (val1-m1) * (val2-m2)
           res1' = res1 + (val1-m1)^2
           res2' = res2 + (val2-m2)^2
       in (num',(res1',res2'))
     ) (0,(0,0)) v1
{-# INLINE pcc #-}
euclideanDistance v1 v2 = sqrt $ UV.sum $ UV.map (^2) $ UV.zipWith (-) v1 v2
{-# INLINE euclideanDistance #-}
corr v1 v2 = 1 - pcc v1 v2
{-# INLINE corr #-}

euclidAndCorr v1 v2 =
    let d1 = euclideanDistance v1 v2
        dm = 0.5 * (sqrt (UV.sum (UV.map (^2) v1)) + sqrt (UV.sum (UV.map (^2) v2)))
    in ((d1 / dm) + (0.5 * corr v1 v2)) / 2
