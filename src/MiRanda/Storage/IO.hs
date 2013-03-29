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

module MiRanda.Storage.IO where

import           Bio.Seq.EMBL
import qualified Codec.Compression.GZip as GZ
import           Control.Arrow
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Function
import qualified Data.HashMap.Strict as H
import           Data.List
import           MiRanda.Binary
import           MiRanda.BranchLen
import           MiRanda.IO (mkProcess,toUTRs,orgToTaxID)
import qualified MiRanda.IO as MI
import           MiRanda.Score
import           MiRanda.Storage.Type
import           MiRanda.Storage.Util
import           MiRanda.Types
import qualified MiRanda.Types as MT
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process
import           MiRanda.Parser
import           Data.Attoparsec.ByteString.Lazy
import           Data.Binary.Builder
import           Data.Binary
import           Control.Parallel.Strategies
import qualified Data.Binary.Get as G
import Data.IORef

runMiRanda :: FilePath -> FilePath -> IO [MRecord]
runMiRanda miFasta utrFasta = do
    proP <- mkProcess miFasta utrFasta
    (_,Just outH,_,pH) <- createProcess proP
    return . MI.mRecordFilter . toMRecords . preprocess =<< L8.hGetContents outH
    -- ex <- waitForProcess pH
    -- case ex of
    --     ExitSuccess -> return mRs
    --     _ -> exitWith ex

filterEMBL :: ByteString -> [EMBL] -> [EMBL]
filterEMBL spe = filter ((== spe) . taxonomy . identification)

dumpDB :: String -> FilePath -> FilePath -> FilePath -> IO ()
dumpDB spe utrFile miRBase outDB = do
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
    
    L.readFile miRBase >>=
        L8.writeFile miFasta . toFastas .
        emblsToMiRNAs . filterEMBL spe3 . extractEMBL . GZ.decompress

    L.readFile utrFile >>=
        L8.writeFile utrFasta . MI.toFastas .
        filter ((== orgToTaxID spe) . taxonomyID) . toUTRs . GZ.decompress

    mRs <- runMiRanda miFasta utrFasta


    utrss <- L.readFile utrFile >>=
             return .
             groupBy ((==) `on` refSeqID) . toUTRs . GZ.decompress
    
    miRHash <- L.readFile miRBase >>=
               return . H.fromList .
               map (identity &&& id) .
               emblsToMiRNAs . filterEMBL spe3 . extractEMBL . GZ.decompress
    L.writeFile outDB $
    -- mapM_ (\v -> do
    --         c <- readIORef counter
                 
    --         print c
    --         writeIORef counter (c+1)
    --       ) $
    --     byteStringToGRs $
    --     GZ.decompress $
        GZ.compressWith GZ.defaultCompressParams {GZ.compressLevel = GZ.bestCompression} $
        L.concat $ map encode $
--        withStrategy (evalBuffer 1 rdeepseq) $ 
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
        
        -- (forM utrss $ \us -> do
        --     let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) us
        --         u = case lhs of
        --             [] -> error $ "dumpDB : no utr is" ++ spe
        --             (u':_) -> u'
        --         binIdx = snd $ getBranchLength (u,rhs)
        --         exp = if "NM_" `B8.isPrefixOf` (refSeqID u)
        --               then Coding
        --               else NonCoding
        --         gInfo = GI (Gene (geneSymbol u) (refSeqID u)) exp u rhs

        --     L8.writeFile utrFasta $ MI.toFastas [u]
    
        --     runMiRanda miFasta utrFasta >>=
        --         return . GR gInfo .
        --         map
        --         (\mr ->
        --           let mRNAL = mRNALen mr
        --               mir = _miRNA mr
        --               ss = MI.toSites mRNAL u $ MT.sites mr
        --               cs = getConservation binIdx u rhs ss
        --           in MiRSites (miRHash H.! mir) $ map toSite $ zip ss cs)
        -- )
            

    
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
               ) .
             dropWhile
             (/= "Current Settings:") .
             L8.lines . L8.filter (/= '\r')

byteStringToGRs :: L.ByteString -> [GeneRecord]
byteStringToGRs b = 
    let bs = L.toChunks b
    in if null bs
       then []
       else go (G.runGetIncremental get `G.pushChunk` (head bs)) (tail bs)
    where
      go r [] =
          case r of
              G.Done unused _ gr ->
                  if B.null unused
                  then [gr]
                  else gr : go (G.runGetIncremental get `G.pushChunk` (B.append unused B.empty)) []
              G.Partial f -> go (f Nothing) []
              G.Fail _ _ _ -> error "parse failed in last record"
      go r (b:bs) =
          case r of
              G.Done unused _ gr ->
                  gr: go (G.runGetIncremental get `G.pushChunk` (B.append unused b)) bs
              G.Partial f ->
                  go (f (Just b)) bs
              G.Fail _ _ _ -> error "parse failed"
              
