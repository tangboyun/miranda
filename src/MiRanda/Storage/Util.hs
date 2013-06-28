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

import           Bio.Seq.EMBL 
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified MiRanda.Storage.Type as ST
import           MiRanda.Storage.Type hiding (seqdata,accessions)
import qualified MiRanda.Types as MT
import           MiRanda.Util (renderFastas)
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)

emblsToMiRNAs :: HashMap ByteString (ByteString,ByteString) -> [EMBL] -> [MiRNA]
emblsToMiRNAs miHash es =
    concatMap
    (\e ->
      let s = case seqdata e of
              CS _ -> error "error emblsToMiRNAs: not SQ"
              SQ _ b -> b
          acc = head . accessions $ e
          fs = filter ((== "miRNA") . key) $ fromMaybe [] $ features e
          rf = case dbCrossRef e of
              Nothing ->
                  if H.member acc miHash
                  then Just $ uncurry (Family "") $ miHash H.! acc
                  else Nothing
              Just rs ->
                  case find ((== "RFAM") . externalDB) rs of
                      Nothing ->
                          if H.member acc miHash
                          then Just $ uncurry (Family "") $ miHash H.! acc
                          else Nothing
                      Just (DBCrossRef _ rID fName) ->
                          if H.member acc miHash
                          then return $ uncurry (Family rID) $ miHash H.! acc
                          else return $ Family rID "" $ fromMaybe "" fName
      in map (extractFromFeature rf s) fs
      ) es
    where
      extractFromFeature rfam str f =
          let vM = M.fromList $ valuePairs f
              (beg,end) = case B8.readInt $ locationStr f of
                  Just (i,str1) -> case B8.readInt $ B8.dropWhile (== '.') str1 of
                      Just (j,"") -> (i-1,j)
                      Nothing -> error "error extractFromFeature: parsing j"
                  Nothing -> error "error extractFromFeature: parsing i"
              removeP s = B8.filter (/= '"') s
          in MiRNA
             (removeP $ vM M.! "product") 
             (removeP $ vM M.! "accession")
             (vM M.! "evidence" == "experimental")
             rfam
             (B8.take (end-beg) $ B8.drop beg str)

toFastas :: [MiRNA] -> L8.ByteString
toFastas = renderFastas . map
           (\miR -> MT.Fasta (identity miR) $ MT.SD $ ST.seqdata miR)
                    
toSite :: (MT.Site,MT.Conservation) -> Site
toSite (s,c) = Site
  { miRandaScore = MT.miRandaScore s
  , conserveScore = c
  , rawScore = MT.rawScore s
  , contextScore = MT.contextScore s
  , contextScorePlus = MT.contextScorePlus s
  , seedRange = MT.seedMatchRange s
  , siteRange = MT.utrRange s
  , seed = MT.seedType s
  , alignStructure = MT.align s
  } 
