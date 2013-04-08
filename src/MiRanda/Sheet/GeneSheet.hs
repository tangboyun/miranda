{-# LANGUAGE OverloadedStrings #-}
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

module MiRanda.Sheet.GeneSheet
       (
           mkGeneWorkbook
       ) 
       where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Colour.Names
import           MiRanda.Sheet.Styles
import           MiRanda.Storage.Type
import           MiRanda.Types (Gene(..),ContextScorePlus(..),ContextScore(..),Conservation(..),SeedType(..),MScore(..))
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
import Control.Applicative
import Data.Maybe

mkGeneWorkbook :: String -> GeneRecord -> Workbook
mkGeneWorkbook dDir r =
    let ge = gene $ geneInfo r
    in addS $
       mkWorkbook $
       [mkWorksheet (Name $ B8.unpack $ ref ge) $ 
        mkTable $
        headLine (B8.unpack $ ref ge) : classLine : mkRow nameCells # withStyleID "bold" :
        (map (toRow dDir ge) $ mirSites r)
       ]
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
              # addStyle (Name "ref") refCell
              
headLine refGeneID =
    mkRow
    [string ("Predicted miRNAs bind to " ++ refGeneID)
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

nameCells = [string "Identity"
            ,string "Accession"
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
            ,string "isExperimentalValidated"
            ,string "Sequence"
            ]


toRow :: String -> Gene -> MiRSites -> Row
{-# INLINE toRow #-}
toRow dDir (Gene s r) miSites =
  let base = B8.unpack
             (mid <> " vs " <>
              r <> "(" <> s <> ")") <.> "pdf"
      path = dDir </> base
      showStr = "Click Me"
      nSite = fromIntegral $ length $ sites miSites
      csp = addScore (fromMaybe 0 . fmap contextPlus . contextScorePlus) $ sites miSites
      c = addScore (fromMaybe 0 . fmap context . contextScore) $ sites miSites
      struct = addScore (structureScore . miRandaScore) $ sites miSites
      free = addScore (freeEnergy . miRandaScore) $ sites miSites
      bl = addScore (branchLength . conserveScore) $ sites miSites
      pc = addScore (fromMaybe 0 . pct . conserveScore) $ sites miSites
      m8c = countSite ((== M8) . seed) (isConserved . conserveScore) $ sites miSites
      m7m8c = countSite ((== M7M8) . seed) (isConserved . conserveScore) $ sites miSites
      m7a1c = countSite ((== M7A1) . seed) (isConserved . conserveScore) $ sites miSites
      m6c = countSite ((== M6) . seed) (isConserved . conserveScore) $ sites miSites
      m6oc = countSite ((== M6O) . seed) (isConserved . conserveScore) $ sites miSites
      imc = countSite ((== Imperfect) . seed) (isConserved . conserveScore) $ sites miSites
      m8n = countSite ((== M8) . seed) (not . isConserved . conserveScore) $ sites miSites
      m7m8n = countSite ((== M7M8) . seed) (not . isConserved . conserveScore) $ sites miSites
      m7a1n = countSite ((== M7A1) . seed) (not . isConserved . conserveScore) $ sites miSites
      m6n = countSite ((== M6) . seed) (not . isConserved . conserveScore) $ sites miSites
      m6on = countSite ((== M6O) . seed) (not . isConserved . conserveScore) $ sites miSites
      imn = countSite ((== Imperfect) . seed) (not . isConserved . conserveScore) $ sites miSites
  in mkRow $
     string (B8.unpack mid) :
     string (B8.unpack a) :
     href path showStr # withStyleID "ref" :
     map myDouble [nSite,csp,c,struct,free,bl,pc] ++
     map (myDouble . fromIntegral) 
     [ m8c,m7m8c,m7a1c,m6c,m6oc,imc,
       m8n,m7m8n,m7a1n,m6n,m6on,imn] ++
     rFamAnnos ++
     [bool b,string (B8.unpack sdat)]
  where
    MiRNA mid a b rfam sdat = mir miSites
    rFamAnnos = case rfam of
        Just (Rfam rID fName) ->
            [href (urlPrefix ++ B8.unpack rID) (B8.unpack rID) # withStyleID "ref"
            ,string (B8.unpack fName)]
        Nothing -> [emptyCell,emptyCell]
    addScore :: (Site -> Double) -> [Site] -> Double
    addScore f = sum . map f
    countSite :: (Site -> Bool) -> (Site -> Bool) -> [Site] -> Int
    countSite f1 f2 = length . filter (liftA2 (&&) f1 f2)
    urlPrefix = "http://rfam.sanger.ac.uk/family/"
    
myDouble :: Double -> Cell
{-# INLINE myDouble #-}
myDouble d = if (snd $ properFraction d) == 0
             then number d
             else number $ read $ printf "%.3f" d
