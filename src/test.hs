{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, GADTs #-}
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

import Diagrams.Prelude hiding (trace)
import Data.Colour
import Data.Colour.Names
-- import MiRanda.Types
-- import MiRanda.Util
import Data.List.Split

import Diagrams.Backend.Cairo.CmdLine
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Data.Char
import Debug.Trace
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Text.Printf
import Data.List
import Data.Function

h = 1
w = 0.6 * h

char ch = text [ch] <> rect w h # lcA transparent
charC c ch = text [ch] # fc c <> rect w h # lcA transparent

string = hcat . map char
stringC c = hcat . map (charC c)

seedColor = blueviolet
bondColor = sienna
siteColor = dodgerblue
seedPairColor = darkorange
sitePairColor = limegreen
data SeedType = M8   -- ^ 8mer site
              | M7M8 -- ^ 7mer-m8 site
              | M7A1 -- ^ 7mer-A1 site
              | M6   -- ^ 6mer site                
              | M6O  -- ^ Offset 6mer site
              | Imperfect -- ^ imperfect seed site
              deriving (Eq,Ord)
                       
lengthOfEach (Align miR3' _ _) =
  let miR = B8.reverse miR3'
      at = UV.unsafeIndex
      idxV = UV.fromList $
             B8.findIndices isAlpha miR
      is = map (idxV `at`) [0,1,7,12,16]
      go [] = []
      go [x] = [B8.drop x miR]
      go (i:j:xs) = (B8.take (j-i) $ B8.drop i miR) : go (j:xs)
  in reverse $ map B8.length $ go is

seedMatchStr l s =
  case s of
    M8 -> siteStr "8mer" ===
          underLine
    M7M8 -> siteStr "7mer-m8" ===
            underLine
    M7A1 -> siteStr "7mer-A1" ===
            underLine
    M6 -> siteStr "6mer" ===
          underLine
    M6O -> siteStr "Offset" ===
           siteStr "6mer" ===
           underLine
    _   -> siteStr "Imperfect" ===
           siteStr "match" ===
           underLine
  where
    siteStr str = stringC siteColor str # centerXY
    underLine = hrule (l * w) # lw 0.05 # lc siteColor

seedStr l = (hrule (l * w) # lw 0.05 # lc seedColor) ===
            (stringC seedColor "Seed" # centerXY)


renderPair3' pair color =
  let 
      pairV = fun pair [16,15,14,13]
      pairD = hcat $
              map (\(c,n) ->
                    let d = if n == 0
                            then mempty
                            else (hcat $ map (charC' color) $ show n) # centerXY
                    in vcat [charC color c
                            ,d]
                  ) $ UV.toList pairV
  in pairD # centerX === hrule (fromIntegral (length pair) * w) # lw 0.05 # lc color
     
fun :: String -> [Int] -> UV.Vector (Char,Int)
fun str ls =
  let strV = (UV.fromList $ map (\c -> (c,0)) str ) :: UV.Vector (Char,Int)
      strI = UV.zip (UV.findIndices (isAlpha.fst) strV) $
              UV.zip (UV.fromList $ filter isAlpha str) $ UV.fromList ls
  in trace (show strV ++ "\n" ++ show strI)  $ UV.update strV strI


renderSeed3' seed =
  let seedV = fun seed [7,6..1]
      seedD = hcat $
              map (\(c,n) ->
                    let d = if n == 0
                            then mempty
                            else (hcat $ map (charC' seedColor) $ show n) # centerXY
                        s = if isAlpha c
                            then charC seedColor c
                            else char c
                    in vcat [s
                            ,d]
                  ) $ UV.toList seedV
  in seedD # centerX === seedStr (fromIntegral $ length seed)

renderSeed5' seed s =
  let seedD = hcat $
              map (\c ->
                    if isAlpha c
                    then charC siteColor c
                    else char c
                    
                  ) $ seed
  in seedD # centerX


charC' c ch = text [ch] # fc c # fontSize 0.5 <> rect (0.4*w) (0.5*h) # lcA transparent

renderMiRNA ali@(Align miR3' utr5' b) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                    B8.unpack miR3'
  in string "     3'-" ||| ((string lhs ||| renderPair3' pair seedPairColor ||| string loop) # centerX ===
      (stringC seedPairColor "3'pairing" # centerXY)) |||
     renderSeed3' seed |||
     string rhs ||| string "-5' miRNA"

renderUTR s ali@(Align miR3' utr5' b) (Pair up dn) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                              B8.unpack utr5'
      rhsD = case s of
               M8 -> (charC red $ head rhs) # alignB
               M7A1 -> (charC red $ head rhs) # alignB
               _    -> string rhs # alignB
      str1 = string (printf "%6d  " up) # alignR ===
             string "5'-" # alignR
      str2 = string (printf " %-6d  " dn) # alignL ===
             string "-3' UTR  " # alignL
  in str1 # alignB ||| (string lhs |||  stringC sitePairColor pair ||| string loop) # alignB |||
     (seedMatchStr (fromIntegral $ length seed) s # centerX ===
      renderSeed5' seed s # centerX) # alignB ||| rhsD ||| str2 # alignB
     

renderBond ali@(Align miR3' utr5' b) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                    B8.unpack b
      renderB = hcat . map (\c ->
                                 if c /= ' '
                                 then charC bondColor c       
                                 else char c)
  in string "        " ||| stringC gray lhs ||| renderB pair ||| stringC gray loop|||
     renderB seed |||
     stringC gray rhs ||| string "         "

data Pair = Pair Int Int
data Align = Align
  { miRNASite3' :: ByteString
  , mRNASite5' :: ByteString
  , hydrogenBond :: ByteString
  } deriving (Show,Eq)


testAlign = Align "NNNNNNNNNNNNNNNNNNNNN"
                  "NNNNNNNNNNNNNNNNNNNNN"
                  "    ||||   || |||||| "

cChar c = (alignedText 0.44 0.44 "C" # fc c <>
           rect 1.12 0.64 # lcA transparent) # centerXY

tChar c = (alignedText 0.45 0.44 "T" # fc c <>
           rect 1.12 0.64 # lcA transparent) # centerXY

gChar c = (alignedText 0.45 0.44 "G" # fc c <>
           rect 1.12 0.64  # lcA transparent) # centerXY

aChar c = (alignedText 0.49 0.44 "A" # fc c <>
           rect 1.12 0.64 # lcA transparent) # centerXY
          
data Sta where
  A ::
    { per :: Double} -> Sta
  T :: 
    { per :: Double} -> Sta
  G :: 
    { per :: Double} -> Sta
  C :: 
    { per :: Double} -> Sta
              

testStr1 = "GGACCAUAGGAAUGAAAACUGCUUU----GCUCAAGUUCCUGUUCCACAGACUCAGGAUU--CCAUACAGAAAG--G------------------------GUUUAUGUCUUUCCA-AAAAUUGAUGAAUAAACUC-CUCUU--CUGG---------UCAAUCUC"
testStr2 = "AGGCUACAUCAACAAAGACUGUUCU----GCUACAGCGCCUGU------------GGAGU--AUACAAGAAAAG--A------------------------GUUUUAUC----------GCUUAAUGAAUAAACUGGUUUUU--UUGG---------UCAAACUU"
testStr3 = "AGAUCACAAGGCUGAAAACUGCUUU----GCUGGAG-UCCUGUUU-------UCAGAGCUCCACAGAAGACACA--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAU-UUUUU--CUGG---------UCAAUGUC"
testStr4 = "AGACCACAAGGCUGAAAAGUGCUUU----GCUAGAG-UCCUGUUC-------UCAGAGCUCCACAGAAGACACG--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAC-UUUUU--CUGG---------UCAAUGUC"
testStr5 = "AGACCACAAGGCUGAAAAGUGCUUU----GCUGGAG-UCCUGUUC-------UCAGAGCUCCACAGAAGACACG--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAC-UUUUU--CUGG---------UCAAUGUC"

plotSeqStas = hcat .
              map (plot . sortBy (flip compare `on` per))
  where
    aColor = red
    gColor = green
    cColor = orange
    tColor = blue
    plot = alignB . vcat .
           map (\s ->
                 case s of
                   A p ->
                     if p == 0
                     then mempty
                     else aChar aColor # scaleY (p/0.25)
                   T p ->
                     if p == 0
                     then mempty
                     else tChar tColor # scaleY (p/0.25)
                   C p ->
                     if p == 0
                     then mempty
                     else cChar cColor # scaleY (p/0.25)
                   G p ->
                     if p == 0
                     then mempty
                     else gChar gColor # scaleY (p/0.25))

testVec = V.fromList [testStr1
                     ,testStr2
                     ,testStr3
                     ,testStr4
                     ,testStr5]
       


main = defaultMain $
       plotSeqStas $
       map (\i ->
             let cVec = V.map (\e -> B8.index e i) testVec
                 count c = fromIntegral $ V.length $
                           V.findIndices (== c) cVec
             in [A $ count 'A' / 5
                ,T $ count 'U' / 5
                ,G $ count 'G' / 5
                ,C $ count 'C' / 5
                ]
           ) [0..B8.length testStr1 -1]
