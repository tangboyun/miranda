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
import qualified Data.ByteString.Lazy.Char8 as L8
import System.Environment
import Data.List
import Control.Arrow
import qualified Data.HashMap.Strict as H
import System.FilePath

main = do
    (annoFile:dbFile:_) <- getArgs
    hSID <- L8.readFile annoFile >>=
            return . H.fromList .
            map (((L8.toStrict . (!! 0)) &&& (L8.toStrict . (!! 2))) . L8.split '\t') .
            L8.lines . L8.filter (/= '\r')
    L.readFile dbFile >>= 
        L.writeFile (dbNew dbFile) .
        GZ.compressWith GZ.defaultCompressParams {GZ.compressLevel = GZ.bestCompression} .
        L.concat .
        map
        (encode . f hSID) .
        byteStringToGRs . GZ.decompress
    where
      dbNew fp = addExtension fp "new"
      f h gr@(GR gi@(GI ge@(Gene _ p) NonCoding _ _) _) =
          gr {geneInfo = gi {gene = ge {ref = h H.! p} }}
      f _ gr@(_) = gr    
