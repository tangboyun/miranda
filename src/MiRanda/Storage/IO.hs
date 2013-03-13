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

import MiRanda.IO (mkProcess,toUTRs,orgToTaxID)
import qualified MiRanda.IO as MI
import MiRanda.Storage.Type
import MiRanda.Types
import qualified Codec.Compression.GZip as GZ
import Data.List
import Control.Monad
import Data.Function
import System.IO
import Bio.Seq.EMBL
import MiRanda.Storage.Util
import Control.Arrow
import qualified Data.HashMap.Strict as H


runMiRanda :: FilePath -> FilePath -> IO [MRecord]
runMiRanda miFasta utrFasta = do
    proP <- mkProcess miFasta utrFasta
    (_,Just outH,_,pH) <- createProcess proP
    mRs <- return . mRecordFilter . toMRecords =<< B8.hGetContents outH
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
    
    (miFasta,_) <- openTempFile tmpDir "miRNA.fa"
    (utrFasta,_) <- openTempFile tmpDir "UTR.fa"
    
    L8.readFile miRBase >>=
        L8.writeFile miFasta . toFastas .
        emblsToMiRNAs . filterEMBL spe3 . extractEMBL . GZ.decompress

    miRHash <- L8.readFile miRBase >>=
               return . H.fromList .
               map (identity *** id) .
               emblsToMiRNAs . filterEMBL spe3 . extractEMBL . GZ.decompress

    utrss <- L8.readFile utrFile >>=
             groupBy ((==) `on` refSeqID) . toUTRs . GZ.decompress

    forM utrss $ us -> do
       let (lhs,rhs) = partition ((== orgToTaxID spe) . taxonomyID) utrs
           u = case lhs of
                 [] -> error $ "dumpDB : no utr is" ++ spe
                 (u':_) -> u'
       L8.writeFile utrFasta $ MI.toFastas [u]

       runMiRanda miFasta utrFasta >>=
          return . (\ss -> zip ss (getConservations ss)) . recordFilter . toRecord spe
      
       
