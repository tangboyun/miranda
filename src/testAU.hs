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
import MiRanda.Diagram.LocalAU
import MiRanda.Diagram.Icon
import MiRanda.Diagram.Pos
import MiRanda.Types
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.Cairo.CmdLine
import qualified Data.ByteString.Char8 as B8
import Data.List
import qualified Data.Vector.Unboxed as UV
import Data.Maybe
import Data.ByteString (ByteString)
import MiRanda.Diagram

uSeq = "GGUGGGCAGCGCUGUCCCCGCUGGGACCCCCGCGGGCUGGUGCGCGGAGGGGGCGGGGGUGGGGUGGAGGGGAGGGGGAAGAGGUACAGGCGGUGUCCGCCUCGUUUCUGGUUGGCUCAGCGGGUUUCUAGGGCUGUGACGUCAUGGGGCAGCUUUCGGAAAAUGACGCGAGCUGGCCGGCCGCGGCUGUGCAGCGUGGGGGUGAAGGUGAAGGCUGGGGCUCCUCGCGCACCCGCGCUGGGGGCCCCCGGGUGGGCUAGGCCGGUUCUGAGCAACCCCCACCCGGCUGCAGGGUGUUUUACAAGGUGAAGAGCCGCAGUGCGCAUAUGAAGAGCCACGCAGAGCAGGAGAAGAAGGCUGCAGCGCUGAGGCUGAAGGAGAAAGAGGCCGCUGCUGCCGCCGCCGCCGCCCACCAGCAGGCCCUGCGGGAGGAGAGCGGUGCGGGCGACAAGGGCUGAGCGCGGGAGCCAGGCUGGCCCAGUCCUGGGCCUCGGCCCUUCCCGCACCGCCGCCAGCGCCCGCAGACACCCUGGCAUCUCAAGAGGGAGUGAGGAGAGGAUUGCAGGGACUUUUCCCUGCGAAACAAAUGAGACAAUGACAUAAACGGCUCUUUUAUUUAUGAAGGCCCUGGGAGCAGCGUUAAGGGCUCCAGGAUCCAGCUCUCUUUGCAUUUGGUCUGUCGGAAGCUGUCCUCGUGCUUUCCUGGACCGGGAGAGUCCCGGUCCCCUCGGGAGGGACUCCACCGCCUCUCACACUCCGAUUUCUGCUGCUCUGCUGCCCCGCAGUCUUUUCCCUUUAUUUGCUUCCCCCUCCUCCCCUCGGCCUCCAGGAAGCCACCGUGGCCGGCCAAGCACAAGCUCACCCACUUUGGAGCAGCAUUUCUCCUCCCCAGAGGACCUCGAAGGCUGCAACGGCCCUGCCCCAAGCCCCAGAAAAAGAAACUCUUGGAGGGAGUCUGCGGGCUUCUCUGAGAAGGGGCGCAGCCUGACACCUGUGCUGUCCCUGCAGUCUCUAGGCCUGUCUGAGCUGCUCCCGUGACCGUGUCCUGAGUGGCCACCUCCUCCUUGGGAGGGAGGCCGCGGCAGGUACUCUCCCUUUGCUGCUUGCUGGCGGCCUUGCUAGGGCUAGAAAGUUUUCUCAGCGUUGGGGGAGGGGCUAGCGUUCUUGUCCAAAGCACUCAUCCUGCUGCAGGGAAGGGAGCUGUACGGAGGCUGGAAGCUGCAGGGUUAGGCCCGAGAGAGCUUUUGACAAGCCUGGCUCUUCCUGCUUCCCACCUUUUUGGCUGAAUACCUUUGGGUCCUGGAGGCUGCCAAGGGCGCUUCGCCUUGAGCAGGUCCCGGAGAGGCUCAGCCUGCCCCUCCUGGGGCCAGCCUCCUUGCCUCUGCUCUGACCUCAGCAGCUACACCUAACCCCACCGCACCCUACCCAGUGUCGAUAUCCACAACCCAGCCCCCAGGCCUGCACUUUGGAGAUUUCUGGAUCCUUCCCUGGGAGGGAGAGGUCUCCUAACUCGAUUGGACAGGAGCCUUCAUUCCUUGACUCGUGUCAUUUGGGAGCUAUUUAUUUAUUCUUAGUAUUUAAUUUUUAACAUCCUCUCAGGGACCAGGGGCCUCCUGCUUUUCAGAGGCCCGAGUGCAUUUAUCCCAAAACGAGGCAUCUUGACAUCCCCUAUCCCCACCCCCUAAAUUCCCAAGGCCCUAAGGUCCCUCACCUGGUUGCUCUGGAAGCUCUUGCUGAUAGGAAUGAUAGCAACACUGAAAGAGCGUGGGAAGGUGGAGGGUGUCACCAGACCCACACAGAGCAAGGGAGAUUAAGGCAGUUCUUGCUGCCCCUUCUCUGGCUGUGCUUUGCCUGAGGGAAGAGUUGGCAAGAGGCCAGAAUUCUAGUUUGGUUUAUGGGGCCUGGAUUCUGGCUUGAAGAAGCCAUAUUUGGCAUGGGUGAAGUGAAAGGGCCAGGAGGUUCUGGAGGCCCCUUCACUUCCAGUCCCAUAGCGGGGCUCAGCUAUUUUUCUGAUCUUAUUCAACUUUAGUUGUGCAAAAGAAAUUCUAAGUGAAUGCGCGUAUGACAGGUCAACGUAUUAGGAAGGAGCCAGAAAGUAUUCAUACUGCUACUCCUUAUGAGAUAGCCAAGAAGCAUCAUGUCAGUAACUGGGAGUGGAGAGGGAGUUUUGUUUUGAAACUAUGCAGGUAUUUUUCAGCAUCCAUUUGUACAUUAGUAUGGUUCUACAGGACCCCAGCUGUGGAAGUAACAAAUUUCUUCUCCCUCCAGACUCUUGGAUGACUACCUAGAUCCCCAGCUUACCGGUUAGUUUCCUCCACAGGCUUCCAUGAGCUCAGUGUCCCAAGACAGCAAAAAUCUGGUAUACCCUGUAGAAGCAGGGGGGUAGCAAAUUUAGGCUCUCUUCGAAAUUACUUGUACAAUACGAGUGCCUUCGCAGCUUAGCCCCAAAGUGCUACUUAUGUUUCCCAAGGUCUUUUCUAUUCAAAUCAUAUUUGGUUUCAGGCCCCUUUAUGGGCAUACGGUAACAGAAACCUCCAUCAUUCAUAUGAGACCCUUUUGUCAGUGGUAACAUCUUGUACUUGGAAAAAAAAUUUUUUUUUUUUUUUUUUUUUUUUUUUGAGACAGAGUCUCAUUCUGUCGCCCAGGCUGGAGUGCAGUGGUGUGAUCUUGGCUCACUGCAACCUCCGCCUCUUGGGUUAAAAGAAUUCUCCUGCCUCAGCCUCCUGAGUAGCUGGGAUUAUAGGCACACACCACCACACCCAGCUAAUCUUUUUGUAUUUUUUUUUUUAGUAGAGACAGGGUUUCGCCAUGUCGGCCAGGCUGGUCUUGAACUCCUGACCUCAGGUGGUCCACCCGCCUCGGCCUCCCAAAGUGCUGGCAUUACAGGCGUGAACCACCGUGCCUGGCCGGAAGUCUUUAAAAAAUAAAGUGAUUCUACUCUUCUAAGCUUACAGAGACCAGACCAGGUGAAUGUAACUGGGGAAAAUCAAGAUGGUACCUCUCUGCAUUAUCCCGCCAGACACUGUAUUUUAUGCAUUCAUGUCUAGGAUACAGUGUGAAAAUUAAAAAGUUUAGAGGGCAGAUGCAAUUGUGGCAAGUGACCUGCCAAUAAAGCAGGUGCAGCUAUAGAAGCUGGCAUAGGUAUAUCCUUAAUGGUGCUUUCUCCCUGGGCUUGUCUUUUUGUUGUUUUUUUCCCCCUAUAUUCAGAGCUCCUUGAGAAGUGAUAAACACCUCCAGCUUUCUAACAUCCUCCCCACACCAUCUCACCAUAUCCAUCUCCCAGCAUCCAUCUGCAUUCAGCUAAGGGCGGGAAACUGACCUAGUGCCUGUGUUGCAGACCAUUUCUGAGGUCUCCACCAUCCAAGGAGGCACAGCCGUCAUUACUGUCCUCCAUGCCUUCAGCAGCCCCCCUCACAGCUAAGGUACAUACCACCCCUUCUGCCGCGCCUCCACCCCUGGCACCAAGGUCUUCUGCUGCUUAUGUCUAAAGGGAUCACCUAUAUUUAACUGCCUCAGUGACCUAACCUCUUUCUUCUCAUGUGCCAGAUGUUAAGAUGAAGGAGGAAUACAACACAUACUCAAGCCUCAGCCUGUUUAGUUGUUUUCACUGGGGCUCGCUUUUCUGGGACGGUAUUUAUUAUCAGACUGGCAAGCCUAACUCCAUAGGUUUACAGGAAGUAGGGAUAUUUUUAUAAAACAAUUGUGUCCUCCCCACAUUUUGCUAUGUUAAUAUUUGCUUCUAACAAUUUGCAGCUGUUUCACUUUUUCCUCAUUUGUCUCUAAGUUGAAGGCUUUGUUGGAGGGGACAGAGCACAGGAACAGCCUUGACAGUCUGUAAUUAUUGUACAGAUAUUUUAAUAGCAUAUAAAUAAGUAUAUUCCUUUUAUUUUGAAACAAAAAUGAUCAGACACUGCCUUUUGUGUGUUUGCUGCCUGUGGCAUCCUUUUUUAAAAAGACUGUUACAUAUUAAAAUAGUGUACAUAUAUAAAUAUUACCUCUUUUGCUGUACAGUUGUGAUAGAGACUGAAGAUUUUAUUUUUUGUGUGCUUUUUAUAAGAAAAAAAUUAAUACACUAAAGAAUCUUGCUGAUGUGAUUGUAAUGUACCUAUGUAACUUAUUUACUUUUGAAUGUUCUUCUGUAUCUUUAAACCUUUUAUUAAAUAAGGUUUUAAAAAUUCA"
          
main = defaultMain $
--       plotLocalAU M8 (P 3991 3999) uSeq
--       eqTriangle 10 # transform reflectionY
--       plotPos 300 (P 260 266) # pad 1.05
       hcat tableHeader
--       mAndT
