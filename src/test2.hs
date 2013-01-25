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

module Main where

import MiRanda.Diagram.Alignment
import MiRanda.Types
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.Cairo.CmdLine
import qualified Data.ByteString.Char8 as B8
import Data.List
import qualified Data.Vector.Unboxed as UV
import Data.Maybe
import Data.ByteString (ByteString)
u1 = ["A2M"
       ,"9361"
     --  123456789 
       ,"GGACCAUAGGAAUGAAAACUGCUUU----GCUCAAGUUCCUGUUCCACAGACUCAGGAUU--CCAUACAGAAAG--G------------------------GUUUAUGUCUUUCCA-AAAAUUGAUGAAUAAACUC-CUCUU--CUGG---------UCAAUCUC"]

us = [["A2M"
      ,"9371"
      ,"AGGCUACAUCAACAAAGACUGUUCU----GCUACAGCGCCUGU------------GGAGU--AUACAAGAAAAG--A------------------------GUUUUAUC----------GCUUAAUGAAUAAACUGGUUUUU--UUGG---------UCAAACUU"]
     ,["A2M"
      ,"9544"
      ,"AGAUCACAAGGCUGAAAACUGCUUU----GCUGGAG-UCCUGUUU-------UCAGAGCUCCACAGAAGACACA--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAU-UUUUU--CUGG---------UCAAUGUC"]
     ,["A2M"
      ,"9598"
      ,"AGACCACAAGGCUGAAAAGUGCUUU----GCUAGAG-UCCUGUUC-------UCAGAGCUCCACAGAAGACACG--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAC-UUUUU--CUGG---------UCAAUGUC"]
     ,["A2M"
      ,"9606"
      ,"AGACCACAAGGCUGAAAAGUGCUUU----GCUGGAG-UCCUGUUC-------UCAGAGCUCCACAGAAGACACG--U------------------------GUUUUUGUAUCUUUAAAGACUUGAUGAAUAAACAC-UUUUU--CUGG---------UCAAUGUC"]
     ]

toUtr (gs:tax:sdata:[]) =
  let Just (i,_) = B8.readInt tax
  in UTR gs "" i (GS sdata)
          
main = defaultMain $
       let dM = plotMultiAlign (P 39 46) (P 26 46) (toUtr u1) (map toUtr us)
       in dM

