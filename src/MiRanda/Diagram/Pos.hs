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

module MiRanda.Diagram.Pos
       (
           plotPos
       )
       where


import Diagrams.Prelude
import Data.Colour.Names
import MiRanda.Types


-- | seed match site
plotPos utrLen (P b e) | utrLen >= 68,b >= 30, e <= utrLen - 30 =
    let lhs = alignTR $
              rect 30 h # lcA transparent # fc crimson |||
              rect (fromIntegral b-30) h # lcA transparent # fc lightgrey |||
              rect (p- fromIntegral b) h # lcA transparent # fc steelblue
        rhs = alignTL $
              rect (fromIntegral e-p) h # lcA transparent # fc steelblue |||
              rect (fromIntegral $ utrLen-30-e) h # lcA transparent # fc lightgrey |||
              rect 30 h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
                       | utrLen < 68 =
    let lhs = alignTR $
              rect (fromIntegral b) h # lcA transparent # fc crimson |||
              rect (p- fromIntegral b) h # lcA transparent # fc violet
        rhs = alignTL $
              rect (fromIntegral e-p) h # lcA transparent # fc violet |||
              rect (fromIntegral $ utrLen - e) h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
                       | e <= 30 =
    let lhs = alignTR $
              rect (fromIntegral b) h # lcA transparent # fc crimson |||
              rect (p- fromIntegral b) h # lcA transparent # fc violet
        rhs = alignTL $
              rect (fromIntegral e-p) h # lcA transparent # fc violet |||
              (if e == 30
               then mempty
               else rect (fromIntegral $ 30 - e) h # lcA transparent # fc crimson) |||
              rect (fromIntegral utrLen - 60) h # lcA transparent # fc lightgrey |||
              rect 30 h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
                       | b >= utrLen - 30 =
    let lhs = alignTR $
              rect 30 h # lcA transparent # fc crimson |||
              rect (fromIntegral utrLen - 60) h # lcA transparent # fc lightgrey |||
              (if b == utrLen -30
               then mempty
               else rect (fromIntegral $ b - utrLen-30) h # lcA transparent # fc crimson) |||
              rect (p- fromIntegral b) h # lcA transparent # fc violet
        rhs = alignTL $
              rect (fromIntegral e-p) h # lcA transparent # fc violet |||
              rect (fromIntegral $ utrLen-e) h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
                       | b < 30 && e > 30 =
    let lhs = alignTR $
              rect (fromIntegral b) h # lcA transparent # fc crimson |||
              (if p <= 30
               then rect (p - fromIntegral b) h # lcA transparent # fc violet
               else rect (30 - fromIntegral b) h # lcA transparent # fc violet |||
                    rect (p - 30) h # lcA transparent # fc steelblue)
        rhs = alignTL $
              (if p < 30
               then rect (30-p) h # lcA transparent # fc violet |||
                    rect (fromIntegral e - 30) h # lcA transparent # fc steelblue
               else rect (fromIntegral e - p) h # lcA transparent # fc steelblue) |||
              rect (fromIntegral $ utrLen - 30 - e) h # lcA transparent # fc lightgrey |||
              rect 30 h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
                       | b < utrLen - 30 && e > utrLen - 30 =
    let lhs = alignTR $
              rect 30 h # lcA transparent # fc crimson |||
              rect (fromIntegral b-30) h # lcA transparent # fc lightgrey |||
              (if p <= fromIntegral utrLen - 30
               then rect (p - fromIntegral b) h # lcA transparent # fc steelblue 
               else rect (fromIntegral $ utrLen - 30 - b) h # lcA transparent # fc steelblue |||
                    rect (p - fromIntegral utrLen + 30) h # lcA transparent # fc violet)
        rhs = alignTL $
              if p <= fromIntegral utrLen - 30
              then rect (fromIntegral utrLen - 30 - p) h # lcA transparent # fc steelblue |||
                   rect (fromIntegral $ e - utrLen + 30) h # lcA transparent # fc violet |||
                   rect (fromIntegral $ utrLen - e) h # lcA transparent # fc crimson
              else rect (fromIntegral e - p) h # lcA transparent # fc violet |||
                   rect (fromIntegral $ utrLen - e) h # lcA transparent # fc crimson
    in (tri <> lhs <> rhs) # centerXY
    where 
      h = (fromIntegral utrLen) / 40
      m = utrLen - 60
      p = fromIntegral (b + e) / 2
      w = h
      h' = 3.5 * w   
      tri = alignB $
            eqTriangle w # lcA transparent # fc green # transform reflectionY
            # scaleY (h' / (w / 2 * sqrt 3))
        
