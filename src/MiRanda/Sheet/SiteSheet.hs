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
module MiRanda.Sheet.SiteSheet
       (
           mkSiteWorkbook
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
import Data.Maybe
import System.FilePath
import Data.Monoid

toSiteLines :: [Record] -> [SiteLine]
{-# INLINE toSiteLines #-}
toSiteLines rs = concatMap snd $ getSites $ recordFilter $ zip rs (getConservations rs)


mkSiteWorkbook :: String -> [Record] -> Workbook
mkSiteWorkbook dDir rs | (not $ null rs) =
    let str = B8.unpack miID
        miID = miRNA $ head rs
    in addS $
       mkWorkbook $
       [mkWorksheet (Name str) $ 
        mkTable $
        headLine str : classLine : mkRow nameCells # withStyleID "bold" :
--        (map toRow $ sort $ toRefLines rs)
        (map (toRow dDir) $ toSiteLines rs)        
       ]
                       | otherwise = emptyWorkbook
  where
    addS wb = wb
              # addStyle (Name "bold") boldCell
              # addStyle (Name "title") titleCell
              # addStyle (Name "siteHead") siteHeadCell
              # addStyle (Name "contPlus") contPlusCell
              # addStyle (Name "cont") contCell
              # addStyle (Name "raw") rawCell
              # addStyle (Name "con") conCell
              # addStyle (Name "therm") thermCell
              # addStyle (Name "gene") geneCell


headLine miID = mkRow
                [string ("Predicted Sites for " ++ miID)
                 # mergeAcross (fromIntegral $ length nameCells - 1)
                 # withStyleID "title"
                ]
                
  
classLine = mkRow
            [string "Gene"
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


nameCells = map string
            ["RefSeqID"
            ,"GeneSymbol"
--            ,"UTR length"
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


toRow :: String -> SiteLine -> Row
toRow dDir sl =
  let (Gene s r) = geneID sl
      mid = miRID sl
      base = B8.unpack
             (mid <> " vs " <>
              r <> "(" <> s <> ")") <.> "pdf"
      path = dDir </> base
      showStr = "Click Me"
      csp = context_Score_Plus sl 
      c = context_Score sl
      raw = raw_Score sl
      (MScore stru free) = mirandaScore sl
      (Con bool bl p) = conserve_Score sl
      pc = case p of
          Just p' -> myDouble p'
          Nothing -> emptyCell
      st = seed sl
      (P i j) = siteRange sl
  in mkRow $
     string (B8.unpack r) :
     string (B8.unpack s) :
     href path showStr :
     string (show (i,j)) :
     string (show st) :
     fromMaybe emptyCell (fmap (myDouble . siteTypeContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . pairingContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . localAUContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . positionContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . taContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . spsContribPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . contextPlus) csp) :
     fromMaybe emptyCell (fmap (myDouble . siteTypeContrib) c) :
     fromMaybe emptyCell (fmap (myDouble . pairingContrib) c) :
     fromMaybe emptyCell (fmap (myDouble . localAUContrib) c) :
     fromMaybe emptyCell (fmap (myDouble . positionContrib) c) :
     fromMaybe emptyCell (fmap (myDouble . context) c) :
     myDouble (unPair $ pairingScore raw) :
     myDouble (unAu $ auScore raw) :
     myDouble (unPos $ posScore raw) :
     myDouble stru : myDouble free :
     myDouble bl : pc : string (show bool) : []
     
  
myDouble :: Double -> Cell
myDouble d = if (snd $ properFraction d) == 0
             then number d
             else number $ read $ printf "%.3f" d
