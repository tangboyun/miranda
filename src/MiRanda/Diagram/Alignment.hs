{-# LANGUAGE OverloadedStrings,BangPatterns,FlexibleContexts #-}
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
       (
         plotMultiAlign
       )
       where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (RGB(..),sRGB,toSRGB)
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           Diagrams.Prelude hiding (diff,end,trace)
import           MiRanda.Parameter
import           MiRanda.Util
import           MiRanda.Types
import           Text.Printf
import Diagrams.TwoD.Text
import Data.Monoid (mappend)

aColor = red
gColor = green
cColor = orange
tColor = blue
uColor = blue
seedColor = dodgerblue
siteColor = darkorange

w',w,h,monoW,monoH :: Double
w' = 1.75 * h

h = 0.64
w = 0.6 * h
monoW = 0.70
monoH = 1
serifFont = "Times New Roman"
monoFont = "Arial"

conStr = "Conservation"

serifStr :: (Renderable Text b,Renderable (Path R2) b) => Colour Double -> String -> Diagram b R2
{-# INLINE serifStr #-}
serifStr !c !str =
  let !n = fromIntegral $ length str
      localSize = 0.8
  in text str # font serifFont
              # fontSize localSize
              # fc c <>
     rect (n * 0.5) monoH # lcA transparent

cChar,tChar,uChar,gChar,aChar :: (Renderable Text b,Renderable (Path R2) b) => Colour Double -> Diagram b R2
{-# INLINE cChar #-}
cChar !c = (alignedText 0.44 0.44 "C" # fc c # font monoFont <>
           rect w' h # lcA transparent) # centerXY
          
{-# INLINE tChar #-}
tChar !c = (alignedText 0.45 0.44 "T" # fc c # font monoFont <>
           rect w' h # lcA transparent) # centerXY
{-# INLINE uChar #-}          
uChar !c = (alignedText 0.45 0.44 "U" # fc c # font monoFont <>
           rect w' h # lcA transparent) # centerXY
          
{-# INLINE gChar #-}
gChar !c = (alignedText 0.45 0.44 "G" # fc c # font monoFont <>
           rect w' h # lcA transparent) # centerXY
          
{-# INLINE aChar #-}
aChar !c = (alignedText 0.49 0.44 "A" # fc c # font monoFont <>
            rect w' h # lcA transparent) # centerXY
char :: (Renderable Text b,Renderable (Path R2) b) => Char -> Diagram b R2           
{-# INLINE char #-}
char !ch = text [ch] <>
          rect w h # lcA transparent


charC :: (Renderable Text b,Renderable (Path R2) b) => Colour Double -> Char -> Diagram b R2           
{-# INLINE charC #-}
charC !c !ch = text [ch] # fc c <>
             rect w h # lcA transparent
             
string :: (Renderable Text b,Renderable (Path R2) b) => String -> Diagram b R2            
{-# INLINE string #-}             
string str = hcat $ map char str

stringC :: (Renderable Text b,Renderable (Path R2) b) => Colour Double -> String -> Diagram b R2            
{-# INLINE stringC #-}
stringC c str = hcat $ map (charC c) str

plotSeqStas :: (Renderable Text b,Renderable (Path R2) b, Backend b R2) => Bool -> [(Double,Double,Double,Double,Double)] -> [Diagram b R2]
{-# INLINE plotSeqStas #-}
plotSeqStas isBW ts = map (plot . sortBy (flip compare `on` per) . tpToList) ts
  where
    tpToList (a,c,g,t,u) =
        let entropy f = if f == 0
                        then 0
                        else negate $ f * logBase 2 f
            f_ = 1 - (a+c+g+t+u)
            r = logBase 2 5 - sum (map entropy [a,c,g,t+u,f_])
        in [A $ a * r, C $ c * r, G $ g * r, T $ t*r, U $ u * r]
    toBW = if isBW
           then luminosity
           else id
    luminosity = (\(RGB r g b) ->
                   let c = 0.21 * r + 0.71 * g + 0.07 * b
                   in sRGB c c c
                   ) . toSRGB
    scaleFactor p = 4 * p / logBase 2 5
    plot = centerXY . (<> strutY (4 * h) # alignB) . alignB . vcat .
           map (\s ->
                 case s of
                   A p ->
                     if p < 0.02
                     then strutX w'
                     else aChar (toBW aColor) # scaleY (scaleFactor p)
                   T p ->
                     if p < 0.02
                     then strutX w'
                     else tChar (toBW tColor) # scaleY (scaleFactor p)
                   U p ->
                     if p < 0.02
                     then strutX w'
                     else uChar (toBW uColor) # scaleY (scaleFactor p)
                   C p ->
                     if p < 0.02
                     then strutX w'
                     else cChar (toBW cColor) # scaleY (scaleFactor p)
                   G p ->
                     if p < 0.02
                     then strutX w'
                     else gChar (toBW gColor) # scaleY (scaleFactor p)
               )

{-# INLINE diff #-}
diff :: Pair -> ByteString -> ByteString -> Double
diff (P up dn) !str1' !str2' =
  let !str1 = B8.take (dn - up + 1) $ B8.drop up str1'
      !str2 = B8.take (dn - up + 1) $ B8.drop up str2'
      cToI !c = case c of
                 'A' -> 1 :: Int
                 'C' -> 2
                 'G' -> 3
                 'T' -> 5
                 'U' -> 5
                 '-' -> 7
                 'N' -> 7 -- for 'N'
                 _   -> error $ "diff: Invalid char " ++ [c] ++ "\n" ++
                        "str1': " ++ show str1 ++ "\n" ++
                        "str2': " ++ show str2
      dif !n = case n of
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
     zipWith ((*) `on` (cToI . toUpper))
     (B8.unpack str1) (B8.unpack str2)
     
plotMultiAlign :: (Renderable Text b,Renderable (Path R2) b,Backend b R2) => UTR -> [UTR] -> Pair -> Pair -> Diagram b R2
plotMultiAlign !utr !utrs !seedRange !siteRange =
  let ns = (map (commonName .
                 (taxMap `at`) .
                 taxonomyID) ss) ++ [conStr]
      nameLen = maximum $ map B8.length ns 
      nameStrs = map ((printf ("%" ++ show nameLen ++ "s  ")) . B8.unpack) ns
      names = init nameStrs
      con   = last nameStrs
      utrs' = sortBy
              (\a b ->
                -- 这里可能用个聚类更好些
                (compare `on`
                 ((diff (P seedBeg seedEnd) -- 以seed位点最近邻排序
                   (extractSeq utr)) . extractSeq)) a b `mappend` 
                (compare `on`
                 ((diff (P siteBeg siteEnd) -- 以site位点最近邻排序
                   (extractSeq utr)) . extractSeq)) a b `mappend` 
                (compare `on`
                 ((diff (P exBeg exEnd) -- 以整个plot片段最近邻排序              
                   (extractSeq utr)) . extractSeq)) a b ) utrs
      seqStr = extractSeq utr
      ss = utr:utrs'
      s = UV.postscanl (+) 0 $ UV.fromList $
          map (\c ->
                if c == '-'
                then 0 :: Int
                else 1) $
          B8.unpack $ seqStr
      strLen = UV.maximum s
      seedBeg = fromJust $! UV.findIndex (== (1+beg seedRange)) s
      seedEnd = 1 + (fromJust $! UV.findIndex (== (end seedRange)) s)
      siteBeg = fromJust $! UV.findIndex (== (1+beg siteRange)) s
      siteEnd = 1 + (fromJust $! UV.findIndex (== (end siteRange)) s)
      siteLen = siteEnd - siteBeg
      (!exBeg,!exEnd) = if siteLen < 60
                        then let !b = ceiling $ (60 - fromIntegral siteLen) / 2
                                 !e = floor $ (60 - fromIntegral siteLen) / 2
                             in if siteBeg - b < 0
                                then (0,60)
                                else if siteEnd + e > UV.length s
                                     then (strLen - 60, strLen)
                                     else (siteBeg - b, siteEnd + e)
                        else (siteBeg, siteEnd)
      seedRB = seedBeg - exBeg
      seedRE = seedEnd - exBeg
      siteRB = siteBeg - exBeg
      siteRE = siteEnd - exBeg
      toFivePart = splitPlacesBlanks -- use splitPlacesBlanks for siteLen >= 60
                   [siteRB
                   ,seedRB - siteRB
                   ,seedRE - seedRB
                   ,siteRE - seedRE
                   ,exEnd - siteRE]
      chss = map (toFivePart . B8.unpack) strs
      strs = map (extractStr (exBeg,exEnd) . unGS . alignment) ss
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
                                       ,(B8.length $ myHead strs)-siteRE]
                                       seqStas
      maxH = 4 * h 
      n = fromIntegral $ 1 + length utrs
      seqStas = map
                (\i ->
                  let vec = UV.fromList $
                            map (\s ->
                                   B8.index s i
                                 ) strs
                      count f = fromIntegral $!
                                UV.length $
                                UV.findIndices f vec
                      !a = count ((== 'A') . toUpper) / n
                      !c = count ((== 'C') . toUpper) / n
                      !g = count ((== 'G') . toUpper) / n
                      !t = count ((== 'T') . toUpper) / n
                      !u = count ((== 'U') . toUpper) / n
                  in (a,c,g,t,u)
                 ) [0..(B8.length $ myHead strs)-1]
      at = (IM.!)
      dMatrix = map (plotOneChain siteStas) chss
      fstLine = (\(a:b:c:d:e:[]) ->
                  let dC = serifStr seedColor "Seed" ===
                           hrule (fromIntegral (length c)* monoW)
                           # lw 0.1
                           # lc seedColor # centerX ===
                           strutY 0.5 ===
                           hcat c # centerX
                      dM = serifStr siteColor
                           ("Binding Site (" ++ show (1+beg siteRange) ++
                            ", " ++ show (end siteRange) ++ ")") ===
                           hrule (fromIntegral (length $ b++c++d) * monoW)
                           # lw 0.1
                           # lc siteColor # centerX ===
                           (hcat b # alignB ||| dC # alignB |||
                            hcat d # alignB) # centerX
                  in centerX $
                     hcat a # alignB ||| dM # alignB |||
                     hcat e # alignB
                  ) $
                toFivePart $ myHead dMatrix
      myHead ls = if null ls
                  then error "Alignment.hs"
                  else head ls
      tailLs = map (centerX . hcat) $ tail dMatrix
      charStas = hcat' (CatOpts Distrib monoW Proxy) $
                 plotSeqStas True beforeSite ++
                 plotSeqStas False siteStr ++
                 plotSeqStas True afterSite
      rs = map
           ((\(a,b,c) ->
              printf "%d ~ %d : %d" a b c :: String
            ) . calcRange (exBeg,exEnd)) ss
      rLen = maximum $ map length rs            
      dM = vcat $ fstLine : (tailLs ++ [centerX $ strutY 0.8 === charStas])
      coef = 0.5
      conBar = centerXY $
               hrule 0.2 === vrule (2*h) === hrule 0.2 === vrule (2*h) ===
               hrule 0.2
      lhs = vcat (map
                  (\nm ->
                    text nm # font serifFont <>
                    rect (fromIntegral nameLen * coef) 1
                    # lcA transparent
                  ) names) ===
            (strutY 0.8 ===
             (((text con # font serifFont) <> rect (fromIntegral nameLen * coef) h # lcA transparent
              ) ||| conBar
             )
            )
      rhs = vcat (map
                  (\r ->
                    text r # font serifFont <>
                    rect (fromIntegral rLen * coef) 1
                    # lcA transparent
                  ) rs) === strutY (0.8 + maxH)
  in (lhs # alignB ||| dM # alignB ||| rhs # alignB) # centerXY
   
  where
    {-# INLINE plotOneChain #-}
    plotOneChain !(bp1:bp2:bp3:[]) !(bwBeg:b1:b2:b3:bwEnd:[]) =
      let f a b = map (charCP . get) $ zip a b
          d1 = f b1 bp1
          d2 = f b2 bp2
          d3 = f b3 bp3
      in map charA bwBeg ++ d1 ++ d2 ++ d3 ++ map charA bwEnd
      where
        charA !ch = text [ch] # font monoFont <>
                    rect monoW monoH # lcA transparent
        charCP (!ch,!p) = text [ch] # font monoFont <>
                          rect monoW monoH # lcA transparent
                          # chooseColor
          where
            !chooseColor = case ch of
              'A' -> fcA (aColor `withOpacity` p) 
              'C' -> fcA (cColor `withOpacity` p) 
              'G' -> fcA (gColor `withOpacity` p) 
              'T' -> fcA (tColor `withOpacity` p) 
              'U' -> fcA (uColor `withOpacity` p) 
              '-' -> id
              'N' -> id
              _ -> error "Invalid char in chooseColor"
        get (!ch',(!a,!c,!g,!t,!u)) =
          let !ch = toUpper ch'
          in case ch of
            'A' -> (ch,a)
            'C' -> (ch,c)
            'G' -> (ch,g)
            'T' -> (ch,t)
            'U' -> (ch,u)
            _ -> (ch,0)
    plotOneChain a b = error $ show a ++ "\n\n\n" ++ show b

calcRange :: (Int,Int) -> UTR -> (Int,Int,Int)
{-# INLINE calcRange #-}
calcRange (!i,!j) u = (\s ->
                      let (str1,str2) = B8.splitAt i s
                          (str3,str4) = B8.splitAt (j-i) str2
                          !a = length $!
                               B8.findIndices isAlpha str1
                          !b = length $!
                               B8.findIndices isAlpha str3
                          !c = length $!
                              B8.findIndices isAlpha str4
                      in (a,a+b,a+b+c)
                    ) $ extractSeq u
                  
{-# INLINE toRange #-}                  
toRange (!i,!j) !str =
  let i' = B8.length $ B8.filter isAlpha $ B8.take i str
      n = B8.length $ B8.filter isAlpha $
          B8.take (j-i) $ B8.drop i str
  in (i',i' + n)
