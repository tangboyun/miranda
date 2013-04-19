
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

import MiRanda.CeRNA
import MiRanda.Sheet
import MiRanda.Binary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import System.Directory
import Control.Monad
import Text.XML.SpreadsheetML.Writer
import Data.List
import MiRanda.Types (Gene(..),ContextScorePlus(..),ContextScore(..))
import MiRanda.Storage.Type
import MiRanda.Storage.IO
import qualified Codec.Compression.GZip as GZ
import System.FilePath
import Control.Applicative
import Data.Function
import Data.Maybe

main = do
  fs <- getCurrentDirectory >>= getDirectoryContents >>=
        return . filter ((== ".data") . takeExtension)
  forM_ fs $ \fp -> do
      let sName = B8.pack $ dropExtension fp
      mgr <- L.readFile fp >>=
             return . find ((== sName) . ref . gene . geneInfo) .
             byteStringToGRs . GZ.decompress
      case mgr of
          Nothing -> return ()
          Just gr -> do
             L.readFile fp >>=
                 writeFile (replaceExtension fp "xml") .
                 showSpreadsheet .
                 mkCeRNAWorkbook gr .
                 filter
                 (liftA2 (&&)
                  (sharePercentageGE 0.3 gr)
                  ((/= sName) . ref . gene . geneInfo)
                 ) .
                 filter (not . null . mirSites) .
                 map (liftA2 GR geneInfo
                      (filter (not . null . sites) .
                       map (liftA2 MiRSites mir
                            (filter
                             (liftA2 (&&) -- 每个位点必须 context+ < -0.10 && context < -0.10
                              ((< (-0.10)) . fromMaybe 0 . fmap contextPlus . contextScorePlus) 
                              ((< (-0.10)) . fromMaybe 0 . fmap context . contextScore)
                             ) .
                             sites)) . mirSites)) .
                 byteStringToGRs . GZ.decompress
