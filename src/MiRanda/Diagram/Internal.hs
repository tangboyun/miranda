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



renderBinding s ali@(Align miR3' utr5' b) =
  let seedStr l = (stringC seedColor "Seed" # centerXY) ===
                  (hrule (l * w) # lw 0.05 # lc seedColor)
      seedMatchStr l = (hrule (l * w) # lw 0.05 # lc siteColor) ===
                       (stringC siteColor "Seed" # centerXY) ===
                       (stringC siteColor "match" # centerXY)
      seedMisMatStr l = (hrule (l * w) # lw 0.05) ===
                       (string "Imperfect" # centerXY) ===
                       (string "match" # centerXY)
      pairStr l = (hrule (l * w) # lw 0.05 # lc sitePairColor) ===
                  (stringC sitePairColor "3'" # centerXY) ===
                  (stringC sitePairColor "pairing" # centerXY)
      (lhs:pair:loop:seed:rhs:[]) = splitPlaces (lengthOfEach ali) $
                                    B8.unpack miR3'
      lastPart = hcat [string lhs
                      ,stringC sitePairColor pair
                      ,string loop
                      ,stringC seedColor seed
                      ,string rhs]
  in undefined
