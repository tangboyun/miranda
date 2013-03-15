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

runMiRanda :: FilePath -> FilePath -> IO [MRecord]
runMiRanda miFasta utrFasta = do
    proP <- mkProcess miFasta utrFasta
    (_,Just outH,_,pH) <- createProcess proP
    mRs <- return . MI.mRecordFilter . MI.toMRecords =<< B8.hGetContents outH
    ex <- waitForProcess pH
    case ex of
        ExitSuccess -> return mRs
        _ -> exitWith ex

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

    miRHash <- L.readFile miRBase >>=
               return . H.fromList .
               map (identity &&& id) .
               emblsToMiRNAs . filterEMBL spe3 . extractEMBL . GZ.decompress

    utrss <- L.readFile utrFile >>=
             return .
             groupBy ((==) `on` refSeqID) . toUTRs . GZ.decompress

    L.writeFile outDB . compress =<< 
        (forM utrss $ \us -> do
        
            let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) us
                u = case lhs of
                    [] -> error $ "dumpDB : no utr is" ++ spe
                    (u':_) -> u'
                binIdx = snd $ getBranchLength (u,rhs)
                exp = if "NM_" `B8.isPrefixOf` (refSeqID u)
                      then Coding
                      else NonCoding
                gInfo = GI (Gene (geneSymbol u) (refSeqID u)) exp u rhs

            L8.writeFile utrFasta $ MI.toFastas [u]
    
            runMiRanda miFasta utrFasta >>=
                return . GR gInfo .
                map
                (\mr ->
                  let mRNAL = mRNALen mr
                      mir = _miRNA mr
                      ss = MI.toSites mRNAL u $ MT.sites mr
                      cs = getConservation binIdx u rhs ss
                  in MiRSites (miRHash H.! mir) $ map toSite $ zip ss cs)

        )
            

transMRs :: [[UTR]] -> [MRecord] -> [GeneRecord]
transMRs utrss mRs =
    map
    (\(utrs,mRs) ->
      let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) utrs
          u = head lhs
          binIdx = snd $ getBranchLength (u,rhs)
          exp = if "NM_" `B8.isPrefixOf` (refSeqID u)
                then Coding
                else NonCoding
          gInfo = GI (Gene (geneSymbol u) (refSeqID u)) exp u rhs
      in 
      ) $ go utrss $ groupBy ((==) `on` _mRNA) mRs
  where 
    go _ [] = []
    go [] _ = []
    go (utrs:rss) mRss@(mRs:mss) =
      if refSeqID (head utrs) == _mRNA (head mRs)
      then (utrs,mRs) : go rss mss
      else go rss mRss
