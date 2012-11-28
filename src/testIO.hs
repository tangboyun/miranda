{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
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

module Main where

import System.Environment

import MiRanda.IO
import MiRanda.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import MiRanda.Sheet.TargetSheet
import MiRanda.Diagram.Alignment
import MiRanda.Diagram.Structure
import Diagrams.Prelude hiding (align)
import Diagrams.Backend.Cairo.CmdLine
spec = "Human"
inF = "/home/tangboyun/miRNAPrediction/miRNA.fa"
utrF = "/home/tangboyun/miRNAPrediction/UTR_Sequences.txt"

main = do
 fa <- fmap head $ readFasta inF
 rs <- miRNAPredict spec fa utrF
 
 writeFile "Summary for predicted targets.xml" $ showSpreadsheet $
   mkTargetWorkbook (seqlabel fa) $ rs

 let r = head $ filter ((== "FAS") . gene) rs
     s = head $ predictedSites r
     al = align s
     st = seedType s
     p = (utrRange s)
     d1 = plotMultiAlign (seedMatchRange s) p (utr r) (homoUTRs r)
     d2 = renderBinding st p al
 -- utrs <- readUTRs utrF ["AGL"]
 -- print $ utrs
-- fst $ renderDia Cairo (CairoOptions "FAS Align.pdf" (Width 800) PDF True) d1
 defaultMain d1
 
