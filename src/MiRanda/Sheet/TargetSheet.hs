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
       , mergeScore
       )
       where

import qualified Data.ByteString.Char8 as B8
import           MiRanda.Sheet.Styles
import           MiRanda.Sheet.Template
import           MiRanda.Types
import MiRanda.Util
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import Data.List
import Text.Printf
import System.FilePath
import Data.Monoid
import Data.Maybe
import Control.DeepSeq
import Data.Char

mergeScore :: (Record,[Conservation]) -> RefLine
mergeScore (r,cs) =
    let ss = predictedSites r
        totalM = foldl1' add $ map miRandaScore ss
        totalCon = foldl1' add cs
        totalR = foldl1' add $ map rawScore ss
        totalCS = foldl1' add $ map contextScore ss
        totalCSP = foldl1' add $ map contextScorePlus ss
        totalS = length ss
        (conSite,nonConSite) =
            foldl'
            (\(con@(a1,b1,c1,d1,e1,f1),nonCon@(a2,b2,c2,d2,e2,f2)) (s,c) ->
              if isConserved c
              then let con' = case seedType s of
                           M8 -> (a1+1,b1,c1,d1,e1,f1)
                           M7M8 -> (a1,b1+1,c1,d1,e1,f1)
                           M7A1 -> (a1,b1,c1+1,d1,e1,f1)
                           M6 -> (a1,b1,c1,d1+1,e1,f1)
                           M6O -> (a1,b1,c1,d1,e1+1,f1)
                           Imperfect -> (a1,b1,c1,d1,e1,f1+1)
                   in force $ (con',nonCon)
              else let nonCon' = case seedType s of
                           M8 -> (a2+1,b2,c2,d2,e2,f2)
                           M7M8 -> (a2,b2+1,c2,d2,e2,f2)
                           M7A1 -> (a2,b2,c2+1,d2,e2,f2)
                           M6 -> (a2,b2,c2,d2+1,e2,f2)
                           M6O -> (a2,b2,c2,d2,e2+1,f2)
                           Imperfect -> (a2,b2,c2,d2,e2,f2+1)
                   in force $ (con,nonCon')
            ) ((0,0,0,0,0,0),(0,0,0,0,0,0)) $ zip ss cs
        ut = utr r
        u = B8.filter isAlpha . extractSeq . utr $ r
        ul = B8.length u
        g = Gene (B8.copy $ geneSymbol ut) (B8.copy $ refSeqID ut)
    in force $ RL (B8.copy $ miRNA r) g totalM totalCon totalR totalCS
       totalCSP totalS conSite nonConSite ul u
{-# INLINE mergeScore #-}        



mkTargetWorkbook :: String -> [RefLine] -> Workbook
{-# INLINE mkTargetWorkbook #-}
mkTargetWorkbook dDir rs | (not $ null rs) =
    let targetGeneComment = toTargetGeneComment str
        idx = fromIntegral $ length (filter (== '\n') targetGeneComment)
        str = B8.unpack miID
        miID = rmiRID $ head rs
    in addS $
       mkWorkbook $
       [mkWorksheet (Name str) $ 
        mkTable $
        mkRow [ string targetGeneComment
                # mergeAcross (fromIntegral $ length nameCells - 1)
                # mergeDown (idx - 2)
                # withStyleID "comment"
                # addTextPropertyAtRanges [(0, fromJust $ elemIndex '\n' targetGeneComment)] [Bold, Text $ dfp {size = Just 14}
                                                                                             ]
              ] : zipWith (\i r -> r # begAtIdx i)
        [idx+2..] 
        (headLine str : classLine : mkRow nameCells # withStyleID "bold" :
         (map (toRow dDir) $ sort rs
         )
        )
--        (map (toRow dDir) $ toRefLines rs)        
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
              # addStyle (Name "ref") refCell
              # addStyle (Name "comment") commentCell
              # addStyle (Name "target") targetCell
              
    headLine miID = mkRow
                    [string ("Target genes for " ++ miID)
                     # mergeAcross (fromIntegral $ length nameCells - 1)
                     # withStyleID "head"
                    ]
                
  
    classLine = mkRow
                [string "Target"
                 # withStyleID "target"
                 # mergeAcross 2
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
                 --             # mergeAcross 1
                 # withStyleID "anno"
                ]
    nameCells = [string "Seqname"
                ,string "GeneSymbol"
                ,string "Type" 
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
                 --            ,string "UTR Sequence"
                ]

toRow :: String -> RefLine -> Row
{-# INLINE toRow #-}
toRow dDir rl =
    let (Gene s r) = rgene rl
        mid = rmiRID rl
        base = B8.unpack
               (mid <> " vs " <>
                r <> "(" <> s <> ")") <.> "pdf"
        path = myMakeValid $ dDir ++ "\\" ++ base
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
       smartHRef r : 
       string (B8.unpack s) :
       string (if "NM_" `B8.isPrefixOf` r
               then "Coding"
               else "NonCoding"
              ) :
       href path showStr # withStyleID "ref" :
       number ns : csp : c :
       myDouble stru : myDouble free :
       myDouble bl : pc :
       map (number . fromIntegral) [a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2] ++
       [number $ fromIntegral $ utrLength rl
--       ,string $ B8.unpack $ utrSeq rl
       ]
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
