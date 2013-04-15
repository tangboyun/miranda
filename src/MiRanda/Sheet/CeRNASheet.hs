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

module MiRanda.Sheet.CeRNASheet
       (
           mkCeRNAWorkbook
       )
       where

import MiRanda.CeRNA
import MiRanda.Sheet.Styles
import MiRanda.Storage.Type
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as UV
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import Data.Maybe
import Control.Arrow
import MiRanda.Types (Gene(..))
import Data.List
import Data.Function
import MiRanda.Sheet.Template

miRBaseVersion = "19"



fstLine gr =
    mkRow $
    [ string . B8.unpack . ref . gene . geneInfo $ gr
    , string . B8.unpack . syb . gene . geneInfo $ gr
    , string . show . expressionStyle . geneInfo $ gr
    , emptyCell
    , number . fromIntegral . length . mirSites $ gr
    , number . fromIntegral . sum . map (length . sites) . mirSites $ gr
    ] ++
    map (number . fromIntegral . length . sites) (mirSites gr) 

mkCeRNAWorkbook :: GeneRecord -> [GeneRecord] -> Workbook
mkCeRNAWorkbook gr grs =
    let idx = fromIntegral $ length (filter (== '\n') ceRNAComment)
        ceRNAComment = toCeRNASheetComment miRBaseVersion (B8.unpack . ref . gene . geneInfo $ gr) 6 (length (nameCells gr) - 1)
        ceRNATable = mkWorksheet (Name $ "CeRNAs") $
                     mkTable $
                     mkRow [ string ceRNAComment
                             # mergeAcross (fromIntegral $ length (nameCells gr) - 1)
                             # mergeDown (idx - 2)
                             # withStyleID "comment"
                             # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' ceRNAComment)]
                             [Bold, Text $ dfp {size = Just 14}]
                           ] : zipWith (\i r -> r # begAtIdx i)
                     [idx+2..] 
                     (headLine gr :
                      classLine gr : mkRow (nameCells gr) # withStyleID "bold" :
                      fstLine gr :
                      (map
                       (\((Gene s r, expr),(mu,vi)) ->
                         mkRow $
                         [ string . B8.unpack $ r
                         , string . B8.unpack $ s
                         , string . show $ expr
                         , number mu
                         , number . fromIntegral . UV.length . UV.filter (/= 0) $ vi
                         , number . fromIntegral . UV.sum $ vi
                         ] ++ map (number . fromIntegral) (UV.toList vi)
                         ) $
                       sortBy (flip compare `on` (fst . snd)) $
                       map
                       (((gene . geneInfo) &&& (expressionStyle . geneInfo)) &&&
                        ((fst &&& (fst . snd)) . toLine gr))
                       grs)
                     )
    in addS $
       mkWorkbook $
       [ceRNATable
       ]
  where
    addS wb = wb
              # addStyle (Name "ceRNA") ceRNACell
              # addStyle (Name "mutame") mutameCell
              # addStyle (Name "miR") miRCell
              # addStyle (Name "head") headCell
              # addStyle (Name "bold") boldCell

headLine gr =
    mkRow
    [string ("CeRNAs for " ++ (B8.unpack . ref . gene . geneInfo $ gr))
     # mergeAcross (fromIntegral $ length (nameCells gr) - 1)
     # withStyleID "head"
    ]

classLine gr =
    mkRow
    [string "CeRNA"
     # withStyleID "ceRNA"
     # mergeAcross 2
    ,string "Scores"
     # withStyleID "mutame"
     # mergeAcross 2
    ,string "miRs"
     # mergeAcross (fromIntegral $ length (nameCells gr) - 7) -- context+ context
     # withStyleID "miR"
    ]

nameCells gr =
   map string $
   ["Seqname"
   ,"GeneSymbol"
   ,"Type"
   ,"MuTaMe Score"
   ,"Total miRs"
   ,"Total Sites"
   ] ++
   map (B8.unpack . identity . mir) (mirSites gr)

