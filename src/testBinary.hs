{-# LANGUAGE OverloadedStrings #-}
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

module Main where
import Data.Binary
import MiRanda.Binary
import MiRanda.Storage.IO
import MiRanda.Storage.Type
import MiRanda.Types hiding (gene)
import qualified Codec.Compression.GZip as GZ
import qualified Data.ByteString.Lazy as L
import System.Environment
import Data.List
ids = ["SCARF1","AK091100","AK127534","NR_028076","uc001abs.2","ASHG19A3A008048"]

main = do
    (fp:_) <- getArgs
    L.readFile fp >>= 
        L.writeFile "/tmp/H1110143A.data" .
        GZ.compressWith GZ.defaultCompressParams {GZ.compressLevel = GZ.bestCompression} .
        L.concat . map encode .
        filter (\gr -> (\(Gene sy re) -> any (`elem` ids) [sy,re]) . gene . geneInfo $ gr) . byteStringToGRs . GZ.decompress
