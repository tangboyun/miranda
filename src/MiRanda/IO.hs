{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.IO where

import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Monoid
import           MiRanda.Parser
import           MiRanda.Types

readRecords :: FilePath -> IO [Record]
readRecords fp =
  L8.readFile fp >>= f . parse parseRecords . preprocess
  where
    f (Done _ r) = return r
    f e = error $ show e
    
preprocess :: ByteString -> ByteString
preprocess = L8.intercalate "\n" . filter
             (\l ->
               L8.isPrefixOf "   " l ||
               L8.isPrefixOf ">" l
               ) .
             dropWhile
             (/= "Current Settings:") .
             L8.lines . L8.filter (/= '\r')

toUTRs :: ByteString -> [UTR]
toUTRs = map
         ((\(_:_:syb:tax:sdata:[]) ->
            case L8.readInt tax of
              Just (taxId,_) -> UTR (L8.toStrict syb) taxId (GS $ L8.toStrict sdata)
              _              -> error "Fail in parse UTR sequence."
          ) . (L8.split '\t')) . L8.lines 

toFastas :: [UTR] -> ByteString
toFastas = toLazyByteString . intercalate '\n' .
           map (\utr ->
                 charUtf8 '>' <> byteString (geneSymbol utr) <>
                 charUtf8 '\t' <> intDec (taxonomyID utr) <>
                 charUtf8 '\n' <>
                 alignToSeq utr <>
                 charUtf8 '\n'
               )
  where
    lineLen = 70
    intercalate _ [] = mempty
    intercalate c (b:bs) = b <> charUtf8 c <>
                           intercalate c bs
    alignToSeq = splitEvery 70 . B8.filter isAlpha .
                 (\(GS str) -> str) . alignment
    splitEvery n bstr = let (h,t) = B8.splitAt n bstr
                        in if B8.null t
                           then byteString h
                           else byteString h <>
                                charUtf8 '\n' <>
                                splitEvery n t

countGene :: ByteString -> Int
countGene str = if L8.null str
                then 0
                else go 0 $ L8.findIndices (== '>') str
  where
    go acc [] = acc
    go acc [i] = acc
    go acc (i:j:is) =
      if j - i == 1
      then (acc+1) `seq` go (acc+1) (j:is)
      else go acc (j:is)
