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

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Data.Colour.Names
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           MiRanda.Score
import           MiRanda.Sheet.Styles
import           MiRanda.Storage.Type
import           MiRanda.Types (Gene(..),ContextScorePlus(..),ContextScore(..),Conservation(..),SeedType(..),MScore(..),RawScore(..),Pair(..),PairScore(..),AUScore(..),PosScore(..))
import           MiRanda.Util
import           System.FilePath
import           Text.Printf
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import MiRanda.Sheet.Template
import Control.Arrow
import Data.Function

miRBaseVersion = "19"

mkGeneWorkbook :: String -> GeneRecord -> Workbook
mkGeneWorkbook dDir r =
    let ge = gene $ geneInfo r
        str1 = toGeneSheetComment miRBaseVersion (B8.unpack $ ref ge)
        str2 = toSiteSheetComment miRBaseVersion (B8.unpack $ ref ge)
        idx1 = fromIntegral $ length (filter (== '\n') str1)
        idx2 = fromIntegral $ length (filter (== '\n') str2)
        geneTable = mkWorksheet (Name $ "miRNAs") $ 
                    mkTable $
                    mkRow [ string str1
                            # mergeAcross (fromIntegral $ length nameCells - 1) 
                            # mergeDown (idx1 - 2)
                            # withStyleID "comment"
                            # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' str1)]
                            [Bold, Text $ dfp {size = Just 14}]
                          ] : zipWith (\i r -> r # begAtIdx i)
                    [idx1+2..] 
                    (headLine (B8.unpack $ ref ge) : classLine : mkRow nameCells # withStyleID "bold" :
                     (map (toRow dDir ge) $
                      sortBy (compare `on` (addScore (fromMaybe 0 . fmap contextPlus . contextScorePlus) . sites)) $
                      mirSites r))
        siteTable = mkWorksheet (Name $ "Sites") $
                    mkTable $
                    mkRow [ string str2
                            # mergeAcross (fromIntegral $ length nameCells' - 1)
                            # mergeDown (idx2 - 2)
                            # withStyleID "comment"
                            # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' str2)]
                            [Bold, Text $ dfp {size = Just 14}]
                          ] : zipWith (\i r -> r # begAtIdx i)
                    [idx2+2..] 
                    (headLine' (B8.unpack $ ref ge) : classLine' : mkRow nameCells' # withStyleID "bold" :
                     (toRows dDir ge $
                      sortBy (compare `on` (fromMaybe 0 . fmap contextPlus . contextScorePlus . snd)) $
                      concatMap (liftA2 zip (repeat . mir) sites) $
                      mirSites r
                     ))
    in addS $
       mkWorkbook $
       [geneTable
       ,siteTable
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
              # addStyle (Name "comment") commentCell
              # addStyle (Name "title") titleCell
              # addStyle (Name "siteHead") siteHeadCell
              # addStyle (Name "contPlus") contPlusCell
              # addStyle (Name "cont") contCell
              # addStyle (Name "raw") rawCell
              # addStyle (Name "therm") thermCell
              # addStyle (Name "gene") geneCell
              
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
             # mergeAcross 4
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
            ,string "Rfam"
            ,string "miRBase"
            ,string "Family"
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
     href (mirPrefix ++ B8.unpack a) (B8.unpack a) # withStyleID "ref" :
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
        Just (Family rID mID fName) ->
            [if B8.null rID
             then emptyCell
             else href (rFamPrefix ++ B8.unpack rID) (B8.unpack rID) # withStyleID "ref"
            ,if B8.null mID
             then emptyCell
             else href (mirFamilyPrefix ++ B8.unpack mID) (B8.unpack mID) # withStyleID "ref" 
            ,string (B8.unpack fName)]
        Nothing -> [emptyCell,emptyCell,emptyCell]
    rFamPrefix = "http://rfam.sanger.ac.uk/family/"
    mirFamilyPrefix = "http://www.mirbase.org/cgi-bin/mirna_summary.pl?fam="

mirPrefix = "http://www.mirbase.org/cgi-bin/mature.pl?mature_acc="

addScore :: (Site -> Double) -> [Site] -> Double
addScore f = sum . map f
countSite :: (Site -> Bool) -> (Site -> Bool) -> [Site] -> Int
countSite f1 f2 = length . filter (liftA2 (&&) f1 f2)
    
myDouble :: Double -> Cell
{-# INLINE myDouble #-}
myDouble d = if (snd $ properFraction d) == 0
             then number d
             else number $ read $ printf "%.3f" d


toRows :: String -> Gene -> [(MiRNA,Site)] -> [Row]
toRows dDir (Gene s r) ms =
    map
    (\(MiRNA i a _ _ _, si) ->
      let base = B8.unpack
                 (i <> " vs " <>
                  r <> "(" <> s <> ")") <.> "pdf"
          path = dDir </> base
          showStr = "Click Me"
          p = (beg &&& end) . siteRange $ si
      in mkRow $
         string (B8.unpack i) :
         href (mirPrefix ++ B8.unpack a) (B8.unpack a) # withStyleID "ref" :
         href path showStr # withStyleID "ref" :
         string (show p) :
         string (show $ seed si) :
         zipWith ((fromMaybe emptyCell . ) . fmap)
         [ myDouble . siteTypeContribPlus
         , myDouble . pairingContribPlus
         , myDouble . localAUContribPlus
         , myDouble . positionContribPlus
         , myDouble . taContribPlus
         , myDouble . spsContribPlus
         , myDouble . contextPlus
         ] (repeat (contextScorePlus si)) ++
         zipWith ((fromMaybe emptyCell .) . fmap)
         [ myDouble . siteTypeContrib
         , myDouble . pairingContrib
         , myDouble . localAUContrib
         , myDouble . positionContrib
         , myDouble . context
         ] (repeat (contextScore si)) ++
         ((\(RS p a pos) -> map myDouble [unPair p, unAu a, unPos pos]) $ rawScore si) ++
         ((\(MScore s f) -> map myDouble [s,f]) $ miRandaScore si) ++
         ((\(Con is bran pct) ->
            [myDouble bran
            ,fromMaybe emptyCell $ fmap myDouble pct
            ,bool is]
          ) $ conserveScore si)
    ) ms
    

headLine' refGeneID = mkRow
                [string ("Predicted miRNA Sites on " ++ refGeneID)
                 # mergeAcross (fromIntegral $ length nameCells' - 1)
                 # withStyleID "title"
                ]
  
classLine' = mkRow
            [string "miRNA"
             # mergeAcross 1
             # withStyleID "gene"
            ,string "Site"
             # mergeAcross 2
             # withStyleID "siteHead"             
            ,string "Context+ Score"
             # mergeAcross 6
             # withStyleID "contPlus"             
            ,string "Context Score"
             # mergeAcross 4
             # withStyleID "cont"             
            ,string "Raw Score"
             # mergeAcross 2
             # withStyleID "raw"             
            ,string "Thermodynamics"
             # mergeAcross 1
             # withStyleID "therm"             
            ,string "Conservation"
             # mergeAcross 2
             # withStyleID "con"             
            ]

nameCells' = map string
            ["Identity"
            ,"Accession"
            ,"Diagrams"
            ,"Range"
            ,"Seed Match"
            ,"Site-type Contribution"
            ,"3' Pairing Contribution"
            ,"Local AU Contribution"
            ,"Position Contribution"
            ,"TA Contribution"
            ,"SPS Contribution"
            ,"Context+ Score"
            ,"Site-type Contribution"
            ,"3' Pairing Contribution"
            ,"Local AU Contribution"
            ,"Position Contribution"
            ,"Context Score"
            ,"3' Pairing Contribution"
            ,"Local AU Contribution"
            ,"Position Contribution"
            ,"Structure Score"
            ,"Free Energy"
            ,"Branch Length"
            ,"Pct"
            ,"IsConserved"]
