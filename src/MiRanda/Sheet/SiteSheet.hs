{-# LANGUAGE OverloadedStrings,BangPatterns #-}
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
       , toSiteLines
       )
       where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Colour.Names
import           MiRanda.Sheet.Styles
import           MiRanda.Sheet.Template
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

toSiteLines :: [(Record,[Conservation])] -> [SiteLine]
{-# INLINE toSiteLines #-}
toSiteLines rs = getSites rs


mkSiteWorkbook :: String -> [SiteLine] -> Workbook
{-# INLINE mkSiteWorkbook #-}
mkSiteWorkbook dDir rs | (not $ null rs) =
    let miID = miRID $ head rs
        str = B8.unpack miID
        targetSiteComment = toTargetSiteComment str
        idx = fromIntegral $ length (filter (== '\n') targetSiteComment)
    in addS $
       mkWorkbook $
       [mkWorksheet (Name str) $ 
        mkTable $
        mkRow [ string targetSiteComment
                # mergeAcross (fromIntegral $ length nameCells - 1)
                # mergeDown (idx - 2)
                # withStyleID "comment"
                # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' targetSiteComment)] [Bold, Text $ dfp {size = Just 14}]
              ] : zipWith (\i r -> r # begAtIdx i)
        [idx+2..] 
        (headLine str : classLine : mkRow nameCells # withStyleID "bold" :
         (map (toRow dDir) rs
         ))        
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
              # addStyle (Name "ref") refCell
              # addStyle (Name "comment") commentCell

headLine miID = mkRow
                [string ("Predicted Sites for " ++ miID)
                 # mergeAcross (fromIntegral $ length nameCells - 1)
                 # withStyleID "title"
                ]
                
  
classLine = mkRow
            [string "Gene"
             # mergeAcross 2
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
            ["Seqname"
            ,"GeneSymbol"
            ,"Type"
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
{-# INLINE toRow #-}
toRow !dDir !sl =
  let (Gene s r) = geneID sl
      mid = miRID sl
      base = B8.unpack
             (mid <> " vs " <>
              r <> "(" <> s <> ")") <.> "pdf"
      path = myMakeValid $ dDir </> base
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
     smartHRef r :
     string (B8.unpack s) :
     string (if "NM_" `B8.isPrefixOf` r
             then "Coding"
             else "NonCoding"
            ) :
     href path showStr # withStyleID "ref" :
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
  where 
    smartHRef str = if "NM_" `B8.isPrefixOf` str ||
                        "NR_" `B8.isPrefixOf` str
                    then toNCBILink str
                    else string . B8.unpack $ str
    toNCBILink l = let s = B8.unpack l
                   in href ("http://www.ncbi.nlm.nih.gov/nuccore/" ++ s ++ "?report=genbank") s
                      # withStyleID "ref"
     
  
myDouble :: Double -> Cell
{-# INLINE myDouble #-}
myDouble d = if (snd $ properFraction d) == 0
             then number d
             else number $ read $ printf "%.3f" d
