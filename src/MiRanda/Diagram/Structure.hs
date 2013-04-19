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

module MiRanda.Diagram.Structure
       (
         renderBinding
       )
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
import Text.Printf
import Data.Function


monoFont = "Consolas"

h = 1
w = 0.62 * h

char ch = text [ch] # font monoFont <> rect w h # lcA transparent
charC c ch = text [ch] # font monoFont  # fc c <> rect w h # lcA transparent
charC' c ch = text [ch] # font monoFont # fc c # fontSize 0.5 <> rect (0.4*w) (0.5*h) # lcA transparent
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
    underLine = strutY 0.1 ===
                hrule (l * w) # lw 0.05 # lc siteColor ===
                strutY 0.1



seedStr l = (hrule (l * w) # lw 0.05 # lc seedColor) ===
            strutY 0.2 ===
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
  in pairD # centerX === hrule (fromIntegral (length pair) * w) # lw 0.05 # lc color === strutY 0.2
     
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
  let (lhs:pair:loop:seed:rhs:[]) = splitPlacesBlanks (lengthOfEach ali) $
                                    B8.unpack miR3'
  in string "     3'-" ||| ((string lhs ||| renderPair3' pair seedPairColor ||| string loop) # centerX ===
      (stringC seedPairColor "3'pairing" # centerXY)) |||
     renderSeed3' seed |||
     string rhs ||| string "-5' miRNA"


-- workaround fix
renderUTR s ali@(Align miR3' utr5' b) (P up dn) =
  let (lhs:pair:loop:seed:rhs:[]) =
          case splitPlaces (lengthOfEach ali) $ B8.unpack utr5' of
              match@(a:b:c:d:e:[]) -> match
              _ -> splitPlacesBlanks (lengthOfEach ali) $ B8.unpack utr5'
      myHead ls = if null ls
                  then error "Structure.hs: rendUTR"
                  else head ls
      rhsD = case s of
               M8 ->
                   if null rhs
                   then mempty
                   else (charC red $ myHead rhs) # alignB
               M7A1 ->
                   if null rhs
                   then mempty
                   else (charC red $ myHead rhs) # alignB
               _    -> string rhs # alignB
      str1 = alignB $ string (printf "%6d  " (up+1)) # alignR ===
             string "5'-" # alignR
      str2 = alignB $ string (printf " %-6d  " dn) # alignL ===
             string "-3' UTR  " # alignL
      withLeft = alignB $ beside unit_X (renderSeed5' seed s)
                 (string lhs |||  stringC sitePairColor pair ||| string loop)
      whole = alignB $ seedMatchStr (fromIntegral $ length seed) s # centerX ===
              beside unitX withLeft rhsD 
  in centerXY $ str1 ||| whole ||| str2     

renderBond ali@(Align miR3' utr5' b) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlacesBlanks (lengthOfEach ali) $
                                    B8.unpack b
      renderB = hcat . map (\c ->
                                 if c /= ' '
                                 then charC bondColor c       
                                 else char c)
  in string "        " ||| stringC gray lhs ||| renderB pair ||| stringC gray loop|||
     renderB seed |||
     stringC gray rhs ||| string "         "


renderBinding s p ali =
  centerXY $
  renderUTR s ali p # centerX ===
  renderBond ali # centerX ===
  renderMiRNA ali # centerX


