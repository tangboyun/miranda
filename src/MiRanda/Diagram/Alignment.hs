{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
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
module MiRanda.Diagram.Alignment
       where

import Diagrams.Prelude hiding (diff,beg,end,trace)
import Data.Colour.Names
import MiRanda.Types
import Data.Colour
import Data.Colour.SRGB (RGB(..),sRGB,toSRGB)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import MiRanda.Parameter
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.List.Split
import Data.Function
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Char
import Debug.Trace

aColor = red
gColor = green
cColor = orange
tColor = blue
uColor = blue
seedColor = dodgerblue
siteColor = darkorange

w' = 1.75 * h
h = 0.64
w = 0.6 * h

cChar c = (alignedText 0.44 0.44 "C" # fc c <>
           rect w' h # lcA transparent) # centerXY

tChar c = (alignedText 0.45 0.44 "T" # fc c <>
           rect w' h # lcA transparent) # centerXY
          
uChar c = (alignedText 0.45 0.44 "U" # fc c <>
           rect w' h # lcA transparent) # centerXY

gChar c = (alignedText 0.45 0.44 "G" # fc c <>
           rect w' h # lcA transparent) # centerXY

aChar c = (alignedText 0.49 0.44 "A" # fc c <>
           rect w' h # lcA transparent) # centerXY

char ch = text [ch] <>
          rect w h # lcA transparent

charC c ch = text [ch] # fc c <>
             rect w h # lcA transparent
             
string = hcat . map char

stringC c = hcat . map (charC c)


plotSeqStas isBW = map (plot . sortBy (flip compare `on` per) . tpToList)
  where
    tpToList (a,c,g,t,u) = [A a, C c, G g, T t, U u]
    toBW = if isBW
           then luminosity
           else id
    luminosity = (\(RGB r g b) ->
                   let c = 0.21 * r + 0.71 * g + 0.07 * b
                   in sRGB c c c
                   ) . toSRGB
    plot = alignB . vcat .
           map (\s ->
                 case s of
                   A p ->
                     if p == 0
                     then mempty
                     else aChar (toBW aColor) # scaleY (p/0.25)
                   T p ->
                     if p == 0
                     then mempty
                     else tChar (toBW tColor) # scaleY (p/0.25)
                   U p ->
                     if p == 0
                     then mempty
                     else uChar (toBW uColor) # scaleY (p/0.25)
                   C p ->
                     if p == 0
                     then mempty
                     else cChar (toBW cColor) # scaleY (p/0.25)
                   G p ->
                     if p == 0
                     then mempty
                     else gChar (toBW gColor) # scaleY (p/0.25))


diff :: Pair -> ByteString -> ByteString -> Double
diff (P up dn) str1' str2' =
  let str1 = B8.take (dn - up + 1) $ B8.drop up str1'
      str2 = B8.take (dn - up + 1) $ B8.drop up str2'
      cToI c = case c of
                 'A' -> 1 :: Int
                 'C' -> 2
                 'G' -> 3
                 'T' -> 5
                 'U' -> 5
                 '-' -> 7
                 _   -> error "Invalid char"
      dif n = case n of
               1 -> 0 
               4 -> 0
               9 -> 0
               25 -> 0
               49 -> 0
               3 -> 1
               10 -> 1
               2 -> 2
               15 -> 2
               5 -> 2
               _ -> 3
  in foldl1 (+) $ map dif $
     zipWith ((*) `on` cToI) (B8.unpack str1) (B8.unpack str2)
           
-- plotMultiAlign :: UTR -> [UTR] -> 
plotMultiAlign seedRange siteRange utr utrs =
  let names = map ((\l ->
                     genus l `B8.append`
                     " " `B8.append` species l) .
                   (taxMap `at`) .
                   taxonomyID) ss
      utrs' = sortBy
              (compare `on`
               ((diff siteRange (extractSeq utr)) . extractSeq)) $
              utrs
      seqStr = extractSeq utr
      ss = utr:utrs'
      s = UV.postscanl (+) 0 $ UV.fromList $
          map (\c ->
                if c == '-'
                then 0 :: Int
                else 1) $
          B8.unpack $ seqStr
      seedBeg = fromJust $ UV.find (== (1+beg seedRange)) s
      seedEnd' = fromJust $ UV.find (== (1+end seedRange)) s
      seedEnd = seedEnd' + 1
      siteBeg = fromJust $ UV.find (== (1+beg siteRange)) s
      siteEnd' = fromJust $ UV.find (== (1+end siteRange)) s
      siteEnd = siteEnd' + 1
      siteLen = siteEnd - siteBeg
      (exBeg,exEnd) = if siteLen < 60
                      then let b = ceiling $ (60 - fromIntegral siteLen) / 2
                               e = floor $ (60 - fromIntegral siteLen) / 2
                           in if siteBeg - b >= 0
                              then (siteBeg - b, siteEnd + e)
                              else (0,60)
                      else (siteBeg, siteEnd)
      seedRB = seedBeg - exBeg
      seedRE = seedEnd - exBeg
      siteRB = siteBeg - exBeg
      siteRE = siteEnd - exBeg
      chss = map
             (splitPlaces
              [siteRB
              ,seedRB - siteRB
              ,seedRE - seedRB
              ,siteRE - seedRE
              ,exEnd - siteRE] . B8.unpack) strs
      
      dMatrix = map (plotOneChain siteStas) chss 
      strs = map (extractStr (exBeg,exEnd) . exGS . alignment) ss
      sites = map (extractStr (siteRB,siteRE)) strs
      siteStas = splitPlacesBlanks
                 [seedRB - siteRB
                 ,seedRE - seedRB
                 ,siteRE - seedRE] $
                 take siteLen $
                 drop siteRB $
                 seqStas
      (beforeSite:siteStr:afterSite:[]) = splitPlacesBlanks
                                       [siteRB
                                       ,siteRE - siteRB
                                       ,(B8.length $ head strs)-siteRE]
                                       seqStas
      seqStas = map
                 (\i ->
                   let vec = UV.fromList $
                             map (\s ->
                                   B8.index s i
                                 ) strs
                       count f = fromIntegral $
                                 UV.length $
                                 UV.findIndices f vec
                       n = fromIntegral $ UV.length vec
                       a = count ((== 'A') . toUpper) / n
                       c = count ((== 'C') . toUpper) / n
                       g = count ((== 'G') . toUpper) / n
                       t = count ((== 'T') . toUpper) / n
                       u = count ((== 'U') . toUpper) / n
                   in (a,c,g,t,u)
                 ) [0..(B8.length $ head strs)-1]
      at = (IM.!)
      charStas = plotSeqStas True beforeSite ++
                 plotSeqStas False siteStr ++
                 plotSeqStas True afterSite
  in (dMatrix ++ [charStas])
   
  where 
    plotOneChain (bp1:bp2:bp3:[]) (bwBeg:b1:b2:b3:bwEnd:[]) =
      let f a b = map (charCP . get) $ zip a b
          d1 = f b1 bp1
          d2 = f b2 bp2
          d3 = f b3 bp3
      in map charA bwBeg ++ d1 ++ d2 ++ d3 ++ map charA bwEnd
      where
        charA ch = text [ch] <>
                   rect 0.6 1 # lcA transparent
        charCP (ch,p) = text [ch] <>
                rect 0.6 1 # lcA transparent
                # chooseColor
          where 
            chooseColor = case ch of
              'A' -> fcA (aColor `withOpacity` p) 
              'C' -> fcA (cColor `withOpacity` p) 
              'G' -> fcA (gColor `withOpacity` p) 
              'T' -> fcA (tColor `withOpacity` p) 
              'U' -> fcA (uColor `withOpacity` p) 
              '-' -> id
              _ -> error "Invalid char in chooseColor"

        get (ch',(a,c,g,t,u)) =
          let ch = toUpper ch'
          in case ch of
            'A' -> (ch,a)
            'C' -> (ch,c)
            'G' -> (ch,g)
            'T' -> (ch,t)
            'U' -> (ch,u)
            _ -> (ch,0)
            
extractStr (i,j) = B8.take (j-i) . B8.drop i
exGS = (\(GS str) -> str)
extractSeq = exGS . alignment

toRange (i,j) str =
  let i' = B8.length $ B8.filter isAlpha $ B8.take i str
      n = B8.length $ B8.filter isAlpha $
          B8.take (j-i) $ B8.drop i str
  in (i',i' + n)
   
 
