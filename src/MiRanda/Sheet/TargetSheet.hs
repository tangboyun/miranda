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
module MiRanda.Sheet.TargetSheet where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Colour.Names
import           MiRanda.Sheet.Styles
import           MiRanda.Types
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import Data.List
import Data.Char

mkTargetWorkbook :: ByteString -> [Record] -> Workbook
mkTargetWorkbook miID rs =
  addS $
  mkWorkbook $
  [mkWorksheet (Name $ B8.unpack miID) $ 
   mkTable $
   headLine miID : classLine : mkRow nameCells # withStyleID "bold" :
   map toRow rs
  ]
  where
    addS wb = wb
              # addStyle (Name "bold") boldCell
              # addStyle (Name "head") headCell
              # addStyle (Name "tsScore") tsScoreCell
              # addStyle (Name "miScore") miScoreCell
              # addStyle (Name "seedMatch") seedMatchCell
              # addStyle (Name "anno") annoCell                            
              
headLine miID = mkRow
                [string ("Target genes for " ++ B8.unpack miID)
                 # mergeAcross (fromIntegral $ length nameCells - 1)
                 # withStyleID "head"
                ]
                
  
classLine = mkRow
            [emptyCell
            ,string "TargetScan"
             # mergeAcross 6 -- con site pa au pos ta sps
             # withStyleID "tsScore"
            ,string "miRanda"
             # mergeAcross 1
             # withStyleID "miScore"
            ,string "Seed Match"
             # mergeAcross 6
             # withStyleID "seedMatch"
            ,string "Annotations"
             # mergeAcross 1
             # withStyleID "anno"
            ]

nameCells = [string "GeneSymbol"
            ,string "Context+"
            ,string "Site-Type"
            ,string "3' Pairing"
            ,string "Local AU"
            ,string "Pos Score"
            ,string "TA"
            ,string "SPS"
            ,string "Structure"
            ,string "Energy"
            ,string "Total"
            ,string "8mer"
            ,string "7mer-m8"
            ,string "7mer-A1"
            ,string "6mer"
            ,string "Offset 6mer"
            ,string "Imperfect"
            ,string "UTR Length"
            ,string "UTR Sequence"
            ] 

toRow :: Record -> Row
toRow re =
  let gid = B8.unpack $ gene re
  in mkRow $
     string gid :
     map number (calcTotalScores $ predictedSites re) ++
     map (number . fromIntegral)
     (countSeedMath $ predictedSites re) ++
     getAnnos re

calcTotalScores :: [Site] -> [Double]
calcTotalScores ss =
  (\(c,si,p,l,o,t,sp,s,e) -> [c,si,p,l,o,t,sp,s,e]) $
  foldl' (\(c,si,p,l,o,t,sp,s,e) site ->
           let (c',si',p',l',o',t',sp') =
                 case contextScorePlus site of
                   Nothing -> (0,0,0,0,0,0,0)
                   Just con ->
                     (contextPlus con
                      ,siteTypeContribPlus con
                      ,pairingContribPlus con
                      ,localAUContribPlus con
                      ,positionContribPlus con
                      ,taContribPlus con
                      ,spsContribPlus con)
               newc = c' + c
               newsi = si + si'
               newp = p + p'
               newl = l + l'
               newo = o + o'
               newt = t + t'
               newsp = sp + sp'
               (MScore s' e') = miRandaScore site
               news = s' + s
               newe = e' + e
           in (newc,newsi,newp,newl,newo,newt,newsp,news,newe)
         ) (0,0,0,0,0,0,0,0,0) ss
  
countSeedMath :: [Site] -> [Int]
countSeedMath ss =
  (\(t,m8,m7m8,m7a1,m6,m6o,i) -> [t,m8,m7m8,m7a1,m6,m6o,i]) $
  foldl'
  (\(t,m8,m7m8,m7a1,m6,m6o,i) site ->
    let st = seedType site
    in case st of
      M8 -> (t+1,m8+1,m7m8,m7a1,m6,m6o,i)
      M7M8 -> (t+1,m8,m7m8+1,m7a1,m6,m6o,i)
      M7A1 -> (t+1,m8,m7m8,m7a1+1,m6,m6o,i)
      M6 -> (t+1,m8,m7m8,m7a1,m6+1,m6o,i)
      M6O -> (t+1,m8,m7m8,m7a1,m6,m6o+1,i)
      _ -> (t+1,m8,m7m8,m7a1,m6,m6o,i+1)
    ) (0,0,0,0,0,0,0) ss

getAnnos :: Record -> [Cell]
getAnnos re =
  let se = B8.filter isAlpha $ unGS $ alignment $ utr re
  in [number (fromIntegral $ B8.length se)
     ,string (B8.unpack se)]
     
  
