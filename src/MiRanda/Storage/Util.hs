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

module MiRanda.Storage.Util where

import Bio.Seq.EMBL
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B8
import MiRanda.Storage.Type
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import MiRanda.Util (renderFastas)


emblsToMiRNAs :: [EMBL] -> [MiRNA]
emblsToMiRNAs es =
    concatMap
    (\e ->
      let s = case seqdata e of
              CS _ -> error "error emblsToMiRNAs: not SQ"
              SQ _ b -> b
          fs = filter ((== "miRNA") . key) $ fromMaybe [] $ features e
      in map (extractFromFeature s) fs
      ) es
    where
      extractFromFeature str f =
          let vM = M.fromList $ valuePairs f
              (beg,end) = case B8.readInt $ locationStr f of
                  Just (i,str) -> case B8.readInt $ B8.dropWhile (== '.') str of
                      Just (j,"") -> (i-1,j)
                      Nothing -> error "error extractFromFeature: parsing j"
                  Nothing -> error "error extractFromFeature: parsing i"
              removeP s = B8.filter (/= '"') s
          in MiRNA
             (removeP $ vM M.! "product") 
             (removeP $ vM M.! "accession")
             (vM M.! "evidence" == "experimental")
             (B8.take (end-beg) $ B8.drop beg str)

toFastas :: [MiRNA] -> ByteString
toFastas = renderFastas . map
           (\miR -> Fasta (identity miR) $ SD $ seqdata miR)
                    
