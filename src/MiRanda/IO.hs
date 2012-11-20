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

import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Monoid

toUTRs :: ByteString -> [UTR]
toUTRs = map
         ((\(_:_:syb:tax:sdata:[]) ->
            case L8.readInt tax of
              Just (taxId,_) -> UTR (L8.toStrict syb) taxId (GS $ L8.toStrict sdata)
              _              -> error "Fail in parse UTR sequence."
          ) . (L8.split '\t')) . L8.lines 

toFastas :: [UTR] -> ByteString
toFastas = intercalate '\n' .
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
