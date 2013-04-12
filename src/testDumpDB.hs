{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
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

import System.Environment

import MiRanda.Storage.IO
import MiRanda.IO (mkUTRFile)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import qualified Codec.Compression.GZip as GZip
import Data.Monoid

geneUTR = "/home/tangboyun/miRNAPrediction/UTR_Sequences.txt"
outUTR = "/tmp/tmpUTR.dat.gz"
miRBase = "/home/tangboyun/miRNADB/miRNA.dat.gz"
utrFile = "/tmp/Mouse_lncRNA_V2.dat.gz"
dbFile = "/home/tangboyun/miRNADB/Mouse_lncRNA_V2_miRBase19.data"
main = do 
   (spe:fastaFile:annoFile:_) <- getArgs
   mkUTRFile spe fastaFile annoFile outUTR
   str <- L8.readFile geneUTR 
   L8.readFile outUTR >>= L.writeFile utrFile . GZip.compress . L8.append str . L8.unlines . tail . L8.lines . L8.filter (/= '\r')
   dumpDB spe utrFile miRBase dbFile
   
