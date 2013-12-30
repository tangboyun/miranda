{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
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
module MiRanda.Diagram.LocalAU
       (
           plotLocalAU
       )
       where

import           Control.Lens (set)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Data.Colour.Names
import           Data.Default.Class
import           Diagrams.Prelude
import           MiRanda.Types


monoW = 0.70
monoH = 1
monoFont = "Arial"
serifFont = "Times New Roman"

seedColor = dodgerblue
fColor = limegreen


charC c f ch = text [ch] # fc c # font f <>
             rect monoW monoH # lcA transparent
             
stringM c f = hcat . map (charC c f)
stringS c f str = text str # fc c # font f <>
                  rect (monoW * (fromIntegral $ length str)) monoH # lcA transparent

rectW = monoW / 5
rectH = monoH * 3

re (h,bool) = if bool
              then rect rectW h
                   # lcA transparent
                   # fc red
                   # alignB
              else rect rectW h
                   # lcA transparent
                   # fc black
                   # alignB
    
sepW = monoW * 0.2

-- input Seed site 
plotLocalAU utr st (P up' dn') =
  let up = case st of
          M8 -> up' 
          M7M8 -> up'
          M7A1 -> up' - 1
          M6O -> up'
          M6 -> up'
          Imperfect -> up'
      dn = case st of
          M8 -> dn'
          M7M8 -> dn' - 1
          M7A1 -> dn'
          M6 -> dn'
          M6O -> dn'
          Imperfect -> dn'
      str = map toUpper $ B8.unpack $
            B8.take (dn - up) $ B8.drop up utr
      (us,ds) = let ls = map (1/) [2.0..]
                    ls1 = 1:ls
                    ls2 = 0.5:ls
                in case st of
                  M8 -> (ls1,ls)
                  M7M8 -> (ls1,ls2)
                  M7A1 -> (ls,ls)
                  _   -> (ls,ls2)
      up30 = B8.unpack $ B8.take 30 $
              B8.map ((\c ->
                        if c == 'T'
                        then 'U'
                        else c) . toUpper) $
              B8.reverse $ B8.take up utr
      dn30 = B8.unpack $ B8.take 30 $
              B8.map ((\c ->
                        if c == 'T'
                        then 'U'
                        else c) . toUpper) $
              B8.drop dn utr
      catOptSetteing = set sep sepW $ set catMethod Cat def      
      localU = hcat' catOptSetteing $
               reverse $ map re $
               zip (map (* rectH) us) $
               map (\c ->
                     if c == 'A' || c == 'U'
                     then True
                     else False) up30
      localD = hcat' catOptSetteing $
               map re $ zip (map (* rectH) ds) $
               map (\c ->
                     if c == 'A' || c == 'U'
                     then True
                     else False) dn30
      lhs = localU ||| strutX sepW
      rhs = strutX sepW ||| localD
      middle = stringM seedColor monoFont str # centerX # alignB
      step1 = beside unit_X middle lhs
      step2 = beside unitX step1 rhs
  in centerXY $ step2 ===
     stringS fColor serifFont (show st) # centerX

   -- centerXY $ localU ||| strutX sep |||
   --   (stringM seedColor monoFont str # centerX ===
   --    stringS fColor serifFont (show st) # centerX) # centerXY |||
   --   strutX sep ||| localD
     
