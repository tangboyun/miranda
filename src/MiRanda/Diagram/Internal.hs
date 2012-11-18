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

h = 1
w = 0.6 * h

char ch = text [ch] <> rect w h # lcA transparent
charC c ch = text [ch] # fc c <> rect w h # lcA transparent

string = hcat . map char
stringC c = hcat . map (charC c)

seedColor = sienna
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

pairStr l = (stringC sitePairColor "3'" # centerXY) ===
            (stringC sitePairColor "pairing" # centerXY) ===
            (hrule (l * w) # lw 0.05 # lc sitePairColor)

renderBinding s ali@(Align miR3' utr5' b) =
  let (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                    B8.unpack miR3'
      lastPart = hcat [string lhs
                      ,stringC sitePairColor pair
                      ,string loop
                      ,stringC seedColor seed
                      ,string rhs]
  in undefined
