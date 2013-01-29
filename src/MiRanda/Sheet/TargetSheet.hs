{-# LANGUAGE BangPatterns,OverloadedStrings #-}
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
module MiRanda.Sheet.TargetSheet
       (
           mkTargetWorkbook
       )
       where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Colour.Names
import           MiRanda.Sheet.Styles
import           MiRanda.Types
import MiRanda.Util
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import Data.List
import Data.Char
import Text.Printf
import MiRanda.Score
import System.FilePath
import Data.Monoid

toRefLines :: [Record] -> [RefLine]
toRefLines rs = map mergeScore $ recordFilter $ zip rs (getConservations rs)
{-# INLINE toRefLines #-}    


mkTargetWorkbook :: String -> [Record] -> Workbook
mkTargetWorkbook dDir rs | (not $ null rs) =
    let str = B8.unpack miID
        miID = miRNA $ head rs
    in addS $
       mkWorkbook $
       [mkWorksheet (Name str) $ 
        mkTable $
        headLine str : classLine : mkRow nameCells # withStyleID "bold" :
--        (map (toRow dDir) $ sort $ toRefLines rs)
        (map (toRow dDir) $ toRefLines rs)        
       ]
                         | otherwise = emptyWorkbook
  where
    addS wb = wb
              # addStyle (Name "bold") boldCell
              # addStyle (Name "head") headCell
              # addStyle (Name "site") siteCell
              # addStyle (Name "tsScore") tsScoreCell
              # addStyle (Name "miScore") miScoreCell
              # addStyle (Name "seedMatch") seedMatchCell
              # addStyle (Name "anno") annoCell
              # addStyle (Name "con") conCell
              # addStyle (Name "poor") poorCell
              
headLine miID = mkRow
                [string ("Target genes for " ++ miID)
                 # mergeAcross (fromIntegral $ length nameCells - 1)
                 # withStyleID "head"
                ]
                
  
classLine = mkRow
            [emptyCell
            ,emptyCell
            ,string "Sites"
             # withStyleID "site"
             # mergeAcross 1
            ,string "TargetScan"
             # mergeAcross 1 -- context+ context
             # withStyleID "tsScore"
            ,string "miRanda"
             # mergeAcross 1
             # withStyleID "miScore"
            ,string "Conservation"
             # mergeAcross 1
             # withStyleID "con"
            ,string "Conserved Sites"
             # mergeAcross 5
             # withStyleID "seedMatch"
            ,string "Poorly Conserved Sites"
             # mergeAcross 5
             # withStyleID "poor"
            ,string "Annotations"
             # mergeAcross 1
             # withStyleID "anno"
            ]

nameCells = [string "RefSeqID"
            ,string "GeneSymbol"
            ,string "Diagrams"
            ,string "Total"
            ,string "Context+"
            ,string "Context"
            ,string "Structure"
            ,string "Energy"
            ,string "Branch Length"
            ,string "Pct"
            ,string "8mer"
            ,string "7mer-m8"
            ,string "7mer-A1"
            ,string "6mer"
            ,string "Offset 6mer"
            ,string "Imperfect"
            ,string "8mer"
            ,string "7mer-m8"
            ,string "7mer-A1"
            ,string "6mer"
            ,string "Offset 6mer"
            ,string "Imperfect"
            ,string "UTR Length"
            ,string "UTR Sequence"
            ]


toRow :: String -> RefLine -> Row
toRow dDir rl =
  let (Gene s r) = rgene rl
      mid = rmiRID rl
      base = B8.unpack
             (mid <> " vs " <>
              r <> "(" <> s <> ")") <.> "pdf"
      path = dDir </> base
      showStr = "Click Me"
      ns = fromIntegral $ totalSite rl
      csp = case rcontextScorePlus rl of
          Just cp -> myDouble $ contextPlus cp
          Nothing -> emptyCell
      c = case rcontextScore rl of
          Just cs -> myDouble $ context cs
          Nothing -> emptyCell
      (MScore stru free) = rmirandaScore rl
      (Con _ bl p) = rconserveScore rl
      pc = case p of
          Just p' -> myDouble p'
          Nothing -> emptyCell
      (a1,b1,c1,d1,e1,f1) = conservedSite rl
      (a2,b2,c2,d2,e2,f2) = nonConservedSite rl
  in mkRow $
     string (B8.unpack r) :
     string (B8.unpack s) :
     href path showStr :
     number ns : csp : c :
     myDouble stru : myDouble free :
     myDouble bl : pc :
     map (number . fromIntegral) [a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2] ++
     [number $ fromIntegral $ utrLength rl
     ,string $ B8.unpack $ utrSeq rl]
     
  
myDouble :: Double -> Cell
myDouble d = if (snd $ properFraction d) == 0
             then number d
             else number $ read $ printf "%.3f" d
