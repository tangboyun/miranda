{-# LANGUAGE NoMonomorphismRestriction #-}
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

module MiRanda.Diagram where
import MiRanda.Util
import MiRanda.Diagram.Alignment 
import MiRanda.Diagram.Structure
import MiRanda.Diagram.Pos
import MiRanda.Diagram.Icon
import MiRanda.Types
import Data.Colour.Names
import           Diagrams.Prelude hiding (align)
import qualified Data.ByteString.Char8 as B8
import MiRanda.Util
import Data.Char (isAlpha)
import Data.List
import Data.Function
import MiRanda.Score
import MiRanda.Diagram.LocalAU
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

hW = 0.6
hH = 1

bgColor = aliceblue

tableHeader :: [Diagram Cairo R2]
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
widthA4 = Width 600
heightA4 = Height 840
    
rend :: FilePath -> Diagram Cairo R2 -> IO ()
rend outFile d =
    fst $ renderDia Cairo (CairoOptions outFile widthA4 PDF False) d


recordDiagram :: Record -> Diagram Cairo R2
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
        aPlot = scale (wt / wa) $ vcat' (CatOpts Cat vsep Proxy) as
        wt = width t
        wa = maximum $ map width (as :: [Diagram Cairo R2])
    in pad 1.01 $ t === strutY 1 === aPlot

tableDiagram :: Record -> Diagram Cairo R2
tableDiagram re =
    let ss = sortBy (compare `on` utrRange) $ predictedSites re
        thisUTR = B8.filter isAlpha $ extractSeq $ utr re
        cons = head $ getConservations [re]
        utrLen = B8.length thisUTR
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
               (\s ->
                 pad 1.05 $
                 case contextScorePlus s of
                     Just csp ->
                         if contextPlus csp < 0
                         then mAndT
                         else onlyM
                     Nothing -> onlyM
               ) ss
        (header:ds) = transpose $ map
                      (\col ->
                        let w = maximum $ map (width . fst) col
                        in map (\(d,h) -> (d,(w,h))) col 
                      ) $ transpose $ (zip tableHeader $ map height tableHeader) :
                      (map
                       (\row ->
                         let h = maximum $ map height row
                         in zip row $ repeat h
                       ) $ transpose [col1,col2,col3,col4,col5])
        hs = map (\(d,(w,h)) ->
                   d <>
                   rect w h
                   # lcA transparent
                   # fc darkblue) header
        dss = map (map
                   (\(d,(w,h)) ->
                     d <>
                     rect w h
                     # lcA transparent
                     # fc bgColor
                   )) ds
        vsep = 0.2
        hsep = 0.2
    in centerXY $ vcat' (CatOpts Cat vsep Proxy) $ map (hcat' (CatOpts Cat hsep Proxy)) $ hs : dss

