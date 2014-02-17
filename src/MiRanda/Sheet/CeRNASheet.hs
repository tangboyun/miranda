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

module MiRanda.Sheet.CeRNASheet
       (
           mkCeRNAWorkbook
       )
       where

-- import MiRanda.CeRNA
import MiRanda.Sheet.Styles
import MiRanda.Storage.Type
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as UV
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import Data.Maybe
-- import Control.Arrow
import MiRanda.Types (Gene(..),UTR(..))
import Data.List
import Data.Function
import MiRanda.Sheet.Template

{-# INLINE miRBaseVersion #-}
miRBaseVersion :: String
miRBaseVersion = "19"

mkCeRNAWorkbook :: GeneRecord -> [((Gene,Expression),(Double,UV.Vector Int))] -> Workbook
--mkCeRNAWorkbook :: GeneRecord -> [GeneRecord] -> Workbook
{-# INLINE mkCeRNAWorkbook #-}
mkCeRNAWorkbook gr ds =
--mkCeRNAWorkbook gr grs =
    let idx = fromIntegral $ length (filter (== '\n') ceRNAComment)
        ceRNAComment = toCeRNASheetComment miRBaseVersion (B8.unpack . ref . gene . geneInfo $ gr) 6 (length nameCells - 1)
        ceRNATable = mkWorksheet (Name $ "CeRNAs") $
                     mkTable $
                     mkRow [ string ceRNAComment
                             # mergeAcross (fromIntegral $ length nameCells - 1)
                             # mergeDown (idx - 2)
                             # withStyleID "comment"
                             # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' ceRNAComment)]
                             [Bold, Text $ dfp {size = Just 14}]
                           ] : zipWith (\i r -> r # begAtIdx i)
                     [idx+2..] 
                     (headLine :
                      classLine : mkRow nameCells # withStyleID "bold" :
                      fstLine :
                      (map
                       (\((Gene s r, expr),(mu,vi)) ->
                         mkRow $
                         [ smartHRef . B8.unpack $ r
                         , string . B8.unpack $ s
                         , string . show $ expr
                         , number mu
                         , number . fromIntegral . UV.length . UV.filter (/= 0) $ vi
                         , number . fromIntegral . UV.sum $ vi
                         ] ++ map (number . fromIntegral) (UV.toList vi)
                         ) $
                       sortBy (flip compare `on` (fst . snd))
                       ds
                      )
                     )
    in addS $
       mkWorkbook $
       [ceRNATable
       ]
  where
    fstLine =
        mkRow $
        [ smartHRef . B8.unpack . ref . gene . geneInfo $ gr
        , string . B8.unpack . syb . gene . geneInfo $ gr
        , string . show . expressionStyle . geneInfo $ gr
        , emptyCell
        , number . fromIntegral . length . mirSites $ gr
        , number . fromIntegral . sum . map (length . sites) . mirSites $ gr
        ] ++
        map (number . fromIntegral . length . sites) (mirSites gr)
    taxID = taxonomyID $ thisSpecies $ geneInfo $ gr
    smartHRef str = if "NM_" `isPrefixOf` str ||
                       "NR_" `isPrefixOf` str
                    then toNCBILink str
                    else if taxID /= 9606
                         then string str
                         else if "ENST" `isPrefixOf` str
                              then toENSTLink str
                              else if "uc" `isPrefixOf` str
                                   then toUCSCLink str
                                   else string str
    toENSTLink l = href ("http://www.ensembl.org/Homo_sapiens/Transcript/Summary?db=core;t=" ++ l) l
                      # withStyleID "ref"
    toUCSCLink l = href ("http://genome.ucsc.edu/cgi-bin/hgGene?db=hg19&hgg_gene=" ++ l) l
                      # withStyleID "ref"
    toNCBILink s = href ("http://www.ncbi.nlm.nih.gov/nuccore/" ++ s ++ "?report=genbank") s
                   # withStyleID "ref"
    addS wb = wb
              # addStyle (Name "ceRNA") ceRNACell
              # addStyle (Name "mutame") mutameCell
              # addStyle (Name "miR") miRCell
              # addStyle (Name "head") headCell
              # addStyle (Name "bold") boldCell
              # addStyle (Name "comment") commentCell
              # addStyle (Name "ref") refCell
    headLine =
        mkRow
        [string ("CeRNAs for " ++ (B8.unpack . ref . gene . geneInfo $ gr))
         # mergeAcross (fromIntegral $ length nameCells - 1)
         # withStyleID "head"
        ]
    classLine =
        mkRow
        [string "CeRNA"
         # withStyleID "ceRNA"
         # mergeAcross 2
        ,string "Scores"
         # withStyleID "mutame"
         # mergeAcross 2
        ,string "miRs"
         # mergeAcross (fromIntegral $ length nameCells - 7) -- context+ context
         # withStyleID "miR"
        ]
    nameCells =
        map string $
        ["Seqname"
        ,"GeneSymbol"
        ,"Type"
        ,"MuTaMe Score"
        ,"Total miRs"
        ,"Total Sites"
        ] ++ map (B8.unpack . identity . mir) (mirSites gr)

