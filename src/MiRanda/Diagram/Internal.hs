{-# LANGUAGE NoMonomorphismRestriction #-}
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

module MiRanda.Diagram.Internal
       where

import Diagrams.Prelude
import Data.Colour
import Data.Colour.Names
import MiRanda.Types
import MiRanda.Util
import Data.List.Split
import Data.Char
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector.Unboxed as UV
import Diagrams.Backend.Cairo.CmdLine
import Text.Printf
import Data.Function

h = 1
w = 0.6 * h

char ch = text [ch] <> rect w h # lcA transparent
charC c ch = text [ch] # fc c <> rect w h # lcA transparent
charC' c ch = text [ch] # fc c # fontSize 0.5 <> rect (0.4*w) (0.5*h) # lcA transparent
string = hcat . map char
stringC c = hcat . map (charC c)

seedColor = blueviolet
bondColor = sienna
siteColor = dodgerblue
seedPairColor = darkorange
sitePairColor = limegreen

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
  in UV.update strV strI


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

renderMiRNA ali@(Align miR3' utr5' b) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                    B8.unpack miR3'
  in string "     3'-" ||| ((string lhs ||| renderPair3' pair seedPairColor ||| string loop) # centerX ===
      (stringC seedPairColor "3'pairing" # centerXY)) |||
     renderSeed3' seed |||
     string rhs ||| string "-5' miRNA"

renderUTR s ali@(Align miR3' utr5' b) (P up dn) =
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


renderBinding s p ali =
  renderUTR s ali p # centerX ===
  renderBond ali # centerX ===
  renderMiRNA ali # centerX


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
