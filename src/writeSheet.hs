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

import qualified Codec.Compression.GZip as GZ
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Diagrams.Backend.Cairo
import           Diagrams.Backend.Cairo.Internal
import           Diagrams.Prelude hiding ((<.>),(<>))
import           MiRanda.Binary
import           MiRanda.Diagram
import           MiRanda.IO
import           MiRanda.Sheet
import           MiRanda.Storage.IO
import           MiRanda.Storage.Type
import           MiRanda.Types hiding (gene,sites,contextScorePlus,contextScore)
import           System.Directory
import           System.Environment
import           System.FilePath
import           Text.XML.SpreadsheetML.Writer
import           Bio.Seq.EMBL
import qualified Data.HashSet as S


outUTR = "/tmp/tmpUTR.dat.gz"
miRBase = "/home/tangboyun/miRNADB/miRNA.dat.gz"
miRFam = "/home/tangboyun/miRNADB/miFam.dat.gz"
dbFile = "/tmp/H1110143A.data"
fOrignFile = "/home/tangboyun/miRNADB/microarray_033010_human_lncRNA_V2_transcript.fa"
aOrignFile = "/home/tangboyun/miRNADB/microarray_033010_human_lncRNA_V2_annotation_20120105.txt"
ids = [--"AK127534",
       "NR_028076","uc001abs.2"]
spe = "Human"
fastaFile = "/tmp/test.fa"
annoFile = "/tmp/testAnno.txt"
utrFile = "/tmp/testAnno.dat.gz"
subDir = "Diagrams"

main = do
   L8.readFile fOrignFile >>=
       L8.writeFile fastaFile .
       L8.intercalate "\n" .
       map L8.unlines . filter (\(l:_) -> L8.tail l `elem` ids) .
       groupBy (\_ b -> L8.head b /= '>') .
       filter (not . L8.null) .
       L8.lines . L8.filter (/= '\r')
   L8.readFile aOrignFile >>=
       L8.writeFile annoFile .
       L8.unlines .
       liftA2 (:) head
       (filter (liftA2 (&&) ((> 2) . length) ((`elem` ids) . (!! 2)) . L8.split '\t') . tail) .
       L8.lines . L8.filter (/= '\r')
   
   mkUTRFile spe fastaFile annoFile outUTR
   L.readFile outUTR >>=
       L.writeFile utrFile . GZ.compress
   
   dumpDB spe utrFile miRBase miRFam dbFile
   createDirectoryIfMissing True subDir

   -- L.readFile dbFile >>=
   --     mapM_
   --     (\gr -> do
   --           let gStr = liftA2 (\r s -> B8.unpack $ r <> "(" <> s <> ")") ref syb . gene . geneInfo $ gr
                 
   --           writeFile ((<.> ".xml") . B8.unpack . ref . gene . geneInfo $ gr) $
   --               showSpreadsheet $ mkGeneWorkbook subDir gr
   --           forM_ (liftA2 zip (map (B8.unpack . identity . mir) . mirSites) recordDiagram' gr) $ \(n,d) ->
   --               fst $ renderDia Cairo (CairoOptions (subDir </> (n ++ " vs " ++ gStr) <.> "pdf") (Width 800) PDF False) d
   --     ) .
   --     map (liftA2 GR geneInfo
   --          (filter (not . null . sites) . map (liftA2 MiRSites mir (filter ((< (-0.05)) . fromMaybe 0 . fmap contextPlus . contextScorePlus) . sites)) .
   --           nubBy ((==) `on` (accession . mir)) . mirSites)) .
   --     filter ((`elem` ids) . L8.fromStrict . ref . gene . geneInfo) . byteStringToGRs . GZ.decompress
       
   L.readFile dbFile >>=
       mapM_ 
       (\gr -> do
             let miHash = S.fromList $ map (identity . mir) $ mirSites gr
                 mRHash = S.empty
                 myUTR = "/tmp/utr"
                 mySPE = "Human"
                 tmpUTR = "/tmp/utrTMP"
                 grID = B8.unpack $ ref $ gene $ geneInfo gr
                 gStr = liftA2 (\r s -> B8.unpack $ r <> "(" <> s <> ")") ref syb . gene . geneInfo $ gr
                 
             writeFile ((<.> ".xml") . B8.unpack . ref . gene . geneInfo $ gr) $
                 showSpreadsheet $ mkGeneWorkbook subDir gr
             forM_ (liftA2 zip (map (B8.unpack . identity . mir) . mirSites) recordDiagram' gr) $ \(n,d) ->
                 fst $ renderDia Cairo (CairoOptions (subDir </> (n ++ " vs " ++ gStr) <.> "pdf") (Width 800) PDF False) d
                 
             mkUTRFile
                 mySPE
                 "/home/tangboyun/miRNADB/microarray_033010_human_lncRNA_V2_transcript.fa"
                 "/home/tangboyun/miRNADB/microarray_033010_human_lncRNA_V2_annotation_20120105.txt"
                 myUTR
             str <- L8.readFile "/home/tangboyun/miRNAPrediction/UTR_Sequences.txt"
             L8.readFile myUTR >>= L.writeFile tmpUTR . GZ.compress . L8.append str . L8.unlines . tail . L8.lines . L8.filter (/= '\r')
             dumpDBWithHashPair mySPE tmpUTR miRBase miRFam (miHash,mRHash) ("/tmp" </> grID <.> "data")
       ) .
       map (liftA2 GR geneInfo
            (filter (not . null . sites) .
             map (liftA2 MiRSites mir
                  (filter
                   (liftA2 (&&) -- 每个位点必须 context+ < -0.10 && context < -0.10
                    ((< (-0.10)) . fromMaybe 0 . fmap contextPlus . contextScorePlus) 
                    ((< (-0.10)) . fromMaybe 0 . fmap context . contextScore)
                   ) .
                   sites)) .
             nubBy ((==) `on` (accession . mir)) . mirSites)) .
       filter ((`elem` ids) . L8.fromStrict . ref . gene . geneInfo) . byteStringToGRs . GZ.decompress
   
   
