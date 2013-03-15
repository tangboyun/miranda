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
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8


utrFile = "/tmp/testUTR.txt.tar.gz"
miRBase = "/tmp/miRNA.dat.gz"
outF = "/tmp/Human_lncRNA_V2.data"
main = dumpDB "Human" utrFile miRBase outF
