{-# LANGUAGE OverloadedStrings,BangPatterns #-}
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

module MiRanda.Storage.IO where

import           Bio.Seq.EMBL
import qualified Codec.Compression.GZip as GZ
import           Control.Arrow
import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Function
import qualified Data.HashMap.Strict as H
import           Data.HashMap.Strict (HashMap(..))
import           Data.List
import           MiRanda.Binary 
import           MiRanda.BranchLen
import           MiRanda.IO (mkProcess,toUTRs,orgToTaxID)
import qualified MiRanda.IO as MI
import           MiRanda.Score
import           MiRanda.Storage.Type 
import           MiRanda.Storage.Util
import           MiRanda.Types hiding (gene,sites,contextScorePlus)
import qualified MiRanda.Types as MT
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           MiRanda.Parser
import           Data.Attoparsec.ByteString.Lazy
import           Data.Binary.Builder hiding (toLazyByteString)
import           Data.Binary
import           Control.Parallel.Strategies
import qualified Data.Binary.Get as G
import Data.IORef
import Data.HashSet (HashSet(..))
import qualified Data.HashSet as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import Data.IORef
import Control.DeepSeq
import Data.ByteString.Lazy.Builder
import Data.ByteString.Lazy.Builder.ASCII
import Data.Monoid
import System.FilePath

runMiRanda :: FilePath -> FilePath -> IO [MRecord]
runMiRanda miFasta utrFasta = do
    proP <- mkProcess miFasta utrFasta
    (_,Just outH,_,pH) <- createProcess proP
    return . MI.mRecordFilter . toMRecords . preprocess =<< L8.hGetContents outH

filterEMBL :: ByteString -> [EMBL] -> [EMBL]
filterEMBL spe = filter ((== spe) . taxonomy . identification)

dumpDB :: String -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
dumpDB spe utrFile miRBase miRFam outDB = do
    let spe3 = case map toLower spe of
            "human" -> "HSA"
            "mouse" -> "MMU"
            "rat"   -> "RNO"
            _ -> error "spe not supported"
            
    tmpDir <- getTemporaryDirectory
    
    (miFasta,h1) <- openTempFile tmpDir "miRNA.fa"
    hClose h1
    (utrFasta,h2) <- openTempFile tmpDir "UTR.fa"
    hClose h2

    miHash <- L.readFile miRFam >>=
              return . parseRfam spe3 . GZ.decompress
    
    L.readFile miRBase >>=
    -- 不同前体可能产生相同miRNA成熟体
        L8.writeFile miFasta . toFastas . nubBy ((==) `on` accession) .
        emblsToMiRNAs miHash . filterEMBL spe3 . extractEMBL . GZ.decompress

    L.readFile utrFile >>=
        L8.writeFile utrFasta . MI.toFastas .
        filter ((== orgToTaxID spe) . taxonomyID) . toUTRs . GZ.decompress

    mRs <- runMiRanda miFasta utrFasta


    utrss <- L.readFile utrFile >>=
             return .
             groupBy ((==) `on` refSeqID) . toUTRs . GZ.decompress
    -- should add family info here
    miRHash <- L.readFile miRBase >>=
               return . H.fromList .
               map (identity &&& id) .
               emblsToMiRNAs miHash . filterEMBL spe3 . extractEMBL . GZ.decompress
    L.writeFile outDB $
        GZ.compressWith GZ.defaultCompressParams {GZ.compressLevel = GZ.bestCompression} $
        L.concat $ map encode $
        map
        (\(utrs,mRs) ->
          let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) utrs
              u = head lhs
              binIdx = snd $ getBranchLength (u,rhs)
              exp = if "NM_" `B8.isPrefixOf` (refSeqID u)
                    then Coding
                    else NonCoding
              gInfo = GI (Gene (geneSymbol u) (refSeqID u)) exp u rhs
              gr = GR gInfo $
                   map
                   (\mr ->
                     let mRNAL = mRNALen mr
                         mir = _miRNA mr
                         ss = MI.toSites mRNAL u $ MT.sites mr
                         cs = getConservation binIdx u rhs ss
                     in MiRSites (miRHash H.! mir) $ map toSite $ zip ss cs) mRs
          in gr
        ) $ toPairs utrss $
        groupBy ((==) `on` _mRNA) mRs



dumpDBWithHashPair :: String -> FilePath -> FilePath -> FilePath -> (HashSet ByteString,HashSet ByteString) -> FilePath -> IO ()
dumpDBWithHashPair spe utrFile miRBase miRFam (miHash,mRHash) outDB = do
    let spe3 = case map toLower spe of
            "human" -> "HSA"
            "mouse" -> "MMU"
            "rat"   -> "RNO"
            _ -> error "spe not supported"
        filterMiRNA = if S.null miHash
                      then id
                      else filter ((`S.member` miHash) . identity)
        filterUTR = if S.null mRHash
                    then id
                    else filter ((`S.member` mRHash) . refSeqID)

                         
    tmpDir <- getTemporaryDirectory
    
    (miFasta,h1) <- openTempFile tmpDir "miRNA.fa"
    hClose h1
    (utrFasta,h2) <- openTempFile tmpDir "UTR.fa"
    hClose h2

    miHash <- L.readFile miRFam >>=
              return . parseRfam spe3 . GZ.decompress
    
    L.readFile miRBase >>=
    -- 不同前体可能产生相同miRNA成熟体
        L8.writeFile miFasta . toFastas . filterMiRNA . nubBy ((==) `on` accession) .
        emblsToMiRNAs miHash . filterEMBL spe3 . extractEMBL . GZ.decompress

    L.readFile utrFile >>=
        L8.writeFile utrFasta . MI.toFastas . filterUTR . 
        filter ((== orgToTaxID spe) . taxonomyID) . toUTRs . GZ.decompress

    mRs <- runMiRanda miFasta utrFasta


    utrss <- L.readFile utrFile >>=
             return .
             groupBy ((==) `on` refSeqID) . toUTRs . GZ.decompress
    -- should add family info here
    miRHash <- L.readFile miRBase >>=
               return . H.fromList .
               map (identity &&& id) .
               emblsToMiRNAs miHash . filterEMBL spe3 . extractEMBL . GZ.decompress
    L.writeFile outDB $
        GZ.compressWith GZ.defaultCompressParams {GZ.compressLevel = GZ.bestCompression} $
        L.concat $ map encode $
        map
        (\(utrs,mRs) ->
          let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) utrs
              u = head lhs
              binIdx = snd $ getBranchLength (u,rhs)
              exp = if "NM_" `B8.isPrefixOf` (refSeqID u)
                    then Coding
                    else NonCoding
              gInfo = GI (Gene (geneSymbol u) (refSeqID u)) exp u rhs
              gr = GR gInfo $
                   map
                   (\mr ->
                     let mRNAL = mRNALen mr
                         mir = _miRNA mr
                         ss = MI.toSites mRNAL u $ MT.sites mr
                         cs = getConservation binIdx u rhs ss
                     in MiRSites (miRHash H.! mir) $ map toSite $ zip ss cs) mRs
          in gr
        ) $ toPairs utrss $
        groupBy ((==) `on` _mRNA) mRs




toPairs _ [] = []
toPairs [] _ = []
toPairs (utrs:rss) mRss@(mRs:mss) =
    if refSeqID (head utrs) == _mRNA (head mRs)
    then (utrs,mRs) : toPairs rss mss
    else toPairs rss mRss


toMRecords :: L8.ByteString -> [MRecord]
toMRecords bs =
    case parse parseRecord bs of
        Done bs' r -> r : toMRecords (L8.dropWhile (== '\n') bs')
        re@(Fail bs' _ _) ->
            if L8.all isSpace bs'
            then []
            else error $ show re

preprocess :: L8.ByteString -> L8.ByteString
{-# INLINE preprocess #-}
preprocess = L8.unlines . filter
             (\l ->
               L8.isPrefixOf "   " l ||
               L8.isPrefixOf ">" l
             ) . dropWhile (/= "Current Settings:") .
             L8.lines . L8.filter (/= '\r')

byteStringToGRs :: L.ByteString -> [GeneRecord]
{-# INLINE byteStringToGRs #-}
byteStringToGRs b = go b
  where
    go bs = 
        case G.runGetOrFail get bs of
            Right (b',_,r) -> r : go b'
            Left (b',_,s) ->
                if L.null b'
                then []
                else error s
              
parseRfam :: ByteString -> L8.ByteString -> HashMap ByteString (ByteString,ByteString)
parseRfam spe str =
    let ss = splitRecord $ L8.filter (/= '\r') str
        spe3 = B8.map toLower spe
    in H.fromList $
       concatMap
       ((liftA2 zip
         (map (!! 1) . filter (B8.isPrefixOf spe3 . (!! 2)) . map B8.words . init . drop 2)
         (repeat . (((!! 1) . B8.words . head) &&& ((!! 1) . B8.words . head . tail)))
        ) . filter (not . B8.null) . B8.lines) ss
        
splitRecord :: L8.ByteString -> [ByteString]
splitRecord str =
    if L8.null str
    then []
    else go $ L8.findIndices (== '/') str
  where
    go [] = []
    go (_:[]) = []
    go (i:j:is) =
      if j - i == 1
      then if i > 1 && L8.index str (i-1) == '\n'
           then let (lhs,rhs) = L8.splitAt (j+1) str
                    remain =
                        case L8.dropWhile (/= '\n') rhs of
                            "" -> ""
                            re -> L8.tail re
                in L8.toStrict lhs :
                   splitRecord remain
           else go is
      else go (j:is)
{-# INLINE splitRecord #-}


dbToEnDB :: ByteString -> FilePath -> FilePath -> FilePath -> IO ()
dbToEnDB spe miRBase recordDB outDB = do
    tmpDir <- getTemporaryDirectory
    
    (tmpGN,h1) <- openTempFile tmpDir "tmp.GN"
    hClose h1
    (tmpGS,h2) <- openTempFile tmpDir "tmp.GS"
    hClose h2

    L.readFile recordDB >>=
        mapM_
        (\ gr -> do
              L8.appendFile tmpGN $ L8.append (L8.fromStrict $ ref $ gene $ geneInfo gr) "\n"
              L8.appendFile tmpGS $ L8.append (L8.fromStrict $ syb $ gene $ geneInfo gr) "\n"
        ) . byteStringToGRs

    (ids,accs) <- L.readFile miRBase >>=
                  -- 不同前体可能产生相同miRNA成熟体
                  return . unzip . nubBy ((==) `on` fst) .
                  map (identity &&& accession) .
                  emblsToMiRNAs H.empty . filterEMBL spe . extractEMBL . GZ.decompress

    seqns <- B8.readFile tmpGN >>=
             return . B8.lines
--    removeFile tmpGN
    gss <- B8.readFile tmpGS >>=
           return . B8.lines
--    removeFile tmpGS
    B8.writeFile "testMiR.txt" $ B8.unlines ids
    B8.writeFile "testMiRACC.txt" $ B8.unlines ids
                          
    let seqV = V.fromList seqns
        gsV = V.fromList gss
        idV = V.fromList ids
        accV = V.fromList accs
        nSeq = V.length seqV -- n gene
        nMiR = V.length idV
        mirHash = H.fromList $ V.toList $ V.zip idV (V.enumFromN 0 $ V.length idV)
        geneHash = H.fromList $ V.toList $ V.zip seqV (V.enumFromN 0 $ V.length seqV)
        

    L.readFile recordDB >>=
        mapM_ 
        (\gr -> do
              let j = geneHash `myLookup` (ref . gene . geneInfo $ gr)
                  vs = map
                       (first (mirHash `myLookup`) .
                        ((identity . mir) &&&
                         (((realToFrac . maybe 0 contextPlus . foldl1 add .
                            map contextScorePlus) &&&
                           (fromIntegral . length)) . sites)
                        )) $ mirSites gr
              contV <- UVM.new nSeq
              UVM.set contV 0
              siteV <- UVM.new nSeq
              UVM.set siteV 0
              forM_ vs $ \(i,(co,si)) -> do
                  UVM.write contV j co
                  UVM.write siteV j si
              cV <- UV.unsafeFreeze contV
              sV <- UV.unsafeFreeze siteV
              
              let strC = foldr
                         (\a b ->
                           doubleDec a <> byteString "\t" <> b
                         ) mempty $ UV.toList cV
                  strS = foldr
                         (\a b ->
                           doubleDec a <> byteString "\t" <> b
                         ) mempty $ UV.toList sV
                         
              L8.appendFile (outDB <.> ".site") $ toLazyByteString strS <> "\n"
              L8.appendFile (outDB <.> ".context") $ toLazyByteString strC <> "\n"

        ) . byteStringToGRs
  where
    myLookup h k = if k `H.member` h
                   then h H.! k
                   else error $
                        show k ++ " not in hash"
    
