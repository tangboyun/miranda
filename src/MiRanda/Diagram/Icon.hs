{-# LANGUAGE NoMonomorphismRestriction #-}
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

module MiRanda.Diagram.Icon
       (
         onlyM
       , mAndT
       , true
       , false
       ) 
       where
import Diagrams.Prelude
import Data.Colour.Names


mChar c = text "M" # fc c # bold <>
          roundedRect 1.5 1.5 0.375
          # lw 0.15
          # lc c

tChar c = text "T" # fc c # bold <>
          roundedRect 1.5 1.5 0.375
          # lw 0.15
          # lc c

utf8Font = "DejaVu Sans YuanTi Condensed"

onlyM = mChar forestgreen # centerXY
mAndT = centerXY $ mChar forestgreen ||| strutX 0.25 ||| tChar crimson 

true = scale 2 $ text "✔" # fc forestgreen # font utf8Font <>
       rect 0.7 1 # lcA transparent

false = scale 2 $ text "✘" # fc crimson # font utf8Font <>
        rect 0.7 1 # lcA transparent

