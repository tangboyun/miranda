{-# LANGUAGE NoMonomorphismRestriction,BangPatterns,FlexibleContexts #-}
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

module MiRanda.Diagram
       (
         recordDiagram
       , tableDiagram
       , recordDiagram'
       , tableDiagram'
       , module MiRanda.Diagram.HeatMap
       ) 
       where

import           Control.Lens (set)
import qualified Data.ByteString.Char8 as B8
import           Data.Char (isAlpha)
import           Data.Colour.Names
import           Data.Default.Class
import           Data.Function
import           Data.List
import           Diagrams.Prelude hiding (align)
import           Diagrams.TwoD.Types
import           MiRanda.Diagram.Alignment
import           MiRanda.Diagram.HeatMap
import           MiRanda.Diagram.Icon
import           MiRanda.Diagram.LocalAU
import           MiRanda.Diagram.Pos
import           MiRanda.Diagram.Structure
import           MiRanda.Score
import qualified MiRanda.Storage.Type as ST
import           MiRanda.Types
import           MiRanda.Util
import           Diagrams.TwoD
import           Diagrams.TwoD.Text

hW = 0.6
hH = 1

bgColor = aliceblue

tableHeader :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => [Diagram b R2]
tableHeader =
    map
    (\str ->
      centerXY $
      text str
      # font "Arial" # bold
      # fc white <>
      rect (fromIntegral (length str) * hW) hH
      # lcA transparent
      # fc darkblue
    ) 
    ["2D Structure"
    ,"Local AU"
    ,"Position"
    ,"Conservation"
    ,"Predicted By"
    ]
    
-- 8.27 inch , cairo default dpi = 72
-- widthA4 = Width 600
-- heightA4 = Height 840
    
-- renderPDF :: FilePath -> Diagram Cairo R2 -> IO ()
-- renderPDF outFile d =
--     fst $ renderDia Cairo (CairoOptions outFile widthA4 PDF False) d

-- renderPNG :: FilePath -> Diagram Cairo R2 -> IO ()
-- renderPNG outFile d =
--     fst $ renderDia Cairo (CairoOptions outFile widthA4 PNG False) d

recordDiagram :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => Record -> Diagram b R2
recordDiagram re =
    let ss = sortBy (compare `on` utrRange) $ predictedSites re
        u = utr re
        us = homoUTRs re
        as = map
             (\s ->
               let seedR = seedMatchRange s
                   siteR = utrRange s
               in plotMultiAlign u us seedR siteR # centerXY) ss
        t = tableDiagram re
        vsep = 1
        catOptSetteing = set sep vsep $ set catMethod Cat def              
        aPlot d1 ds = scale (getScaleFactor d1 ds) $ vcat' catOptSetteing ds
        getScaleFactor :: (Renderable Text b,Renderable (Path R2) b,Backend b R2) => Diagram b R2 -> [Diagram b R2] -> Double
        getScaleFactor d1 ds = width d1 / (maximum $ map width ds)
    in pad 1.01 (t === strutY 1 === aPlot t as)

tableDiagram :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => Record -> Diagram b R2
tableDiagram re =
    let (ss,cons) = unzip $ sortBy (compare `on` (utrRange.fst)) $
                    zip (predictedSites re) (myHead $ getConservations [re])
        thisUTR = B8.filter isAlpha $ extractSeq $ utr re
        utrLen = B8.length thisUTR
        myHead ls = if null ls
                    then error "tableDiagram"
                    else head ls
        col1 = map
               (\s ->
                 pad 1.05 $
                 renderBinding (seedType s) (utrRange s) (align s)) ss
        col2 = map
               (\s ->
                 pad 1.05 $
                 plotLocalAU thisUTR (seedType s) (seedMatchRange s)) ss
        col3 = map
               (\s ->
                 pad 1.05 $ scale (10 / fromIntegral utrLen) $
                 plotPos utrLen (seedMatchRange s)) ss
        col4 = map (\c ->
                     if isConserved c
                     then true
                     else false
                   ) cons 
        col5 = map
               (\s -> -- contextScorePlus 有定义，且小于0,3种主要seed match
                 pad 1.05 $
                 case seedType s of
                     M6 -> onlyM
                     M6O -> onlyM
                     Imperfect -> onlyM
                     _ -> case contextScorePlus s of
                         Just csp ->
                             if contextPlus csp < 0
                             then mAndT
                             else onlyM
                         Nothing -> onlyM
               ) ss
        traned :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => [Diagram b R2] -> [[(Diagram b R2,(Double,Double))]]
        traned xs = transpose $ map
                 (\col ->
                   let w = maximum $ map (width . fst) col
                   in map (\(d,h) -> (d,(w,h))) col 
                 ) $ transpose $ (zip xs $ map height xs) :
                 (map
                  (\row ->
                    let h = maximum $ map height row
                    in zip row $ repeat h
                  ) $ transpose [col1,col2,col3,col4,col5])
        result :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => [[(Diagram b R2,(Double,Double))]] -> [[Diagram b R2]]
        result xss = if null xss
                 then []
                 else (map (\(d,(w,h)) ->
                            d <>
                            rect w h
                            # lcA transparent
                            # fc darkblue) $ head xss) :
                      (map (map
                            (\(d,(w,h)) ->
                              d <>
                              rect w h
                              # lcA transparent
                              # fc bgColor
                            )) $ tail xss)
        vsep = 0.2
        hsep = 0.2
        vCatOptSetteing = set sep vsep $ set catMethod Cat def
        hCatOptSetteing = set sep hsep $ set catMethod Cat def
    in centerXY $
       vcat' vCatOptSetteing $
       map (hcat' hCatOptSetteing) $
       result (traned tableHeader)

tableDiagram' :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => ST.GeneInfo -> ST.MiRSites -> Diagram b R2
tableDiagram' gi mirSs =
    let ss = sortBy (compare `on` ST.siteRange) $
             ST.sites mirSs
        thisUTR = B8.filter isAlpha $ extractSeq $ ST.thisSpecies gi
        utrLen = B8.length thisUTR
        col1 = map
               (\s ->
                 pad 1.05 $
                 renderBinding (ST.seed s) (ST.siteRange s) (ST.alignStructure s)) ss
        col2 = map
               (\s ->
                 pad 1.05 $
                 plotLocalAU thisUTR (ST.seed s) (ST.seedRange s)) ss
        col3 = map
               (\s ->
                 pad 1.05 $ scale (10 / fromIntegral utrLen) $
                 plotPos utrLen (ST.siteRange s)) ss
        col4 = map (\s ->
                     if isConserved $ ST.conserveScore s
                     then true
                     else false
                   ) ss
        col5 = map
               (\s -> -- contextScorePlus 有定义，且小于0,3种主要seed match
                 pad 1.05 $
                 case ST.seed s of
                     M6 -> onlyM
                     M6O -> onlyM
                     Imperfect -> onlyM
                     _ -> case ST.contextScorePlus s of
                         Just csp ->
                             if contextPlus csp < 0
                             then mAndT
                             else onlyM
                         Nothing -> onlyM
               ) ss
        traned :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => [Diagram b R2] -> [[(Diagram b R2,(Double,Double))]]
        traned xs = transpose $ map
                 (\col ->
                   let w = maximum $ map (width . fst) col
                   in map (\(d,h) -> (d,(w,h))) col 
                 ) $ transpose $ (zip xs $ map height xs) :
                 (map
                  (\row ->
                    let h = maximum $ map height row
                    in zip row $ repeat h
                  ) $ transpose [col1,col2,col3,col4,col5])
        result :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => [[(Diagram b R2,(Double,Double))]] -> [[Diagram b R2]]
        result xss = if null xss
                 then []
                 else (map (\(d,(w,h)) ->
                            d <>
                            rect w h
                            # lcA transparent
                            # fc darkblue) $ head xss) :
                      (map (map
                            (\(d,(w,h)) ->
                              d <>
                              rect w h
                              # lcA transparent
                              # fc bgColor
                            )) $ tail xss)
        vsep = 0.2
        hsep = 0.2
        vCatOptSetteing = set sep vsep $ set catMethod Cat def
        hCatOptSetteing = set sep hsep $ set catMethod Cat def
    in centerXY $
       vcat' vCatOptSetteing $
       map (hcat' hCatOptSetteing) $ 
       result (traned tableHeader)
       
       
recordDiagram' :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) =>  ST.GeneRecord -> [Diagram b R2]
recordDiagram' gr =
    let gi = ST.geneInfo gr
        u = ST.thisSpecies gi
        us = ST.otherSpecies gi
    in map
       (\mrSite ->
          let ss = sortBy (compare `on` ST.siteRange) $
                   ST.sites mrSite
              as = map
                   (\s ->
                     let seedR = ST.seedRange s
                         siteR = ST.siteRange s
                     in plotMultiAlign u us seedR siteR # centerXY) ss
              t = tableDiagram' gi mrSite
              vsep = 1
              catOptSetteing = set sep vsep $ set catMethod Cat def              
              aPlot d1 ds = if null us
                            then mempty
                            else scale (getScaleFactor d1 ds) $ vcat' catOptSetteing ds
              getScaleFactor :: (Renderable Text b,Renderable (Path R2) b,Backend b R2) => Diagram b R2 -> [Diagram b R2] -> Double
              getScaleFactor d1 ds = width d1 / (maximum $ map width ds)
          in if null ss
             then mempty
             else pad 1.01 (t === strutY 1 === aPlot t as)) $
       ST.mirSites gr
