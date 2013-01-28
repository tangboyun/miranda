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
import MiRanda.Score
import Data.List
import MiRanda.Sheet.TargetSheet
import MiRanda.Sheet.SiteSheet
import MiRanda.Diagram
spec = "Human"
inF = "/home/tangboyun/Dropbox/miRanda Test Data/miRNA.fa"
inF' = "/tmp/testmiR.txt"
-- utrF = "/home/tangboyun/miRNAPrediction/UTR_Sequences.txt"
utrF = "/home/tangboyun/Dropbox/miRanda Test Data/testUTR.txt"
geneIDs = ["LIN28B"]

utrF' = "/tmp/testUTR.txt"

main = do
 --   print $ getAUScoreImpl M7M8 (P 44 51) $ B8.filter (/= '-') utrS
 -- str <- L8.readFile utrF
 -- let ls = L8.lines $ L8.filter (/= '\r') str
 --     str' = L8.intercalate "\n" $ head ls : filter ((`elem` geneIDs).(!! 2) . L8.split '\t') (tail ls)
 -- L8.writeFile utrF' str'
 fas <- readFasta inF
-- rs <- 
 
-- mapM_ (B8.putStrLn) $ toTargetScanOutFormat $ toSiteLines rs

-- miRNAPredict spec (head fas) utrF' >>= writeFile "/tmp/test.xml" . showSpreadsheet . mkSiteWorkbook "hsa-let-7i"
 rs <- miRNAPredict spec (head fas) utrF
 defaultMain $
     renderRecord $ head rs
-- writeFile "Summary for predicted targets.xml" $ showSpreadsheet $
 --   mkTargetWorkbook (seqlabel fa) $ rs

 -- let r = head $ filter ((== "FAS") . gene) rs
 --     s = head $ predictedSites r
 --     al = align s
 --     st = seedType s
 --     p = (utrRange s)
 --     d1 = plotMultiAlign (seedMatchRange s) p (utr r) (homoUTRs r)
 --     d2 = renderBinding st p al
 -- utrs <- readUTRs utrF ["AGL"]
 -- print $ utrs
-- fst $ renderDia Cairo (CairoOptions "FAS Align.pdf" (Width 800) PDF True) d1
-- defaultMain d1
 
