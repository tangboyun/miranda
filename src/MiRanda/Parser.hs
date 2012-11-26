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

module MiRanda.Parser where
import Prelude hiding (take)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.Char
import qualified Data.ByteString.Char8 as B8
import MiRanda.Types
import MiRanda.Util
import Control.Applicative

parseSite :: Parser MSite
parseSite = do
  "   Forward:" .*> skipWhile (/= '\n') *> endOfLine
  s1 <- string "   Query:"
  sp1 <- takeWhile1 (== ' ')
  s2 <- string "3'"
  sp2 <- takeWhile1 (== ' ')
  miR <- takeWhile1 (/= ' ') <* skipWhile (/= '\n') <* endOfLine
  let n = B8.length s1 + B8.length sp1 + B8.length s2 + B8.length sp2
      nB = B8.length miR
  b <- count n (char ' ') *> take nB <* skipWhile (/= '\n') <* endOfLine
  utr <- take n *> take nB <* skipWhile (/= '\n') <* endOfLine
  "   Energy:" .*> skipWhile (/= '\n') *> endOfLine
  _miId <- char '>' *> takeWhile1 (/= '\t') <* char '\t'
  _utrId <- takeWhile1 (/= '\t') <* char '\t'
  structS <- double <* char '\t'
  enS <- double <* char '\t'
  miR1 <- decimal <* char ' '
  miR2 <- decimal <* char '\t'
  utrR1 <- decimal <* char ' '
  utrR2 <- decimal <* char '\t'
  al <- decimal <* char '\t'
  wcRatio <- fmap (* 0.01) $ double <* char '%' <* char '\t'
  guRatio <- fmap (* 0.01) $ double <* char '%' <* char '\n'
  let miRang = P (miR1 - 1) miR2
      utrRang = P (utrR1 - 1) utrR2
      m = Match (round $ fromIntegral al * wcRatio)
          (round $ fromIntegral al * guRatio)
      ali = Align miR utr b
      st = getSeedType ali
  return $ MSite (MScore structS enS) miRang utrRang al m st ali

parseRecord :: Parser MRecord
parseRecord = do
  ss <- many1 parseSite
  miRId <- string ">>" *> takeWhile1 (/= '\t') <* char '\t'
  utrId <- takeWhile1 (/= '\t') <* char '\t'
  totS <- double <* char '\t'
  totE <- double <* char '\t'
  maxS <- double <* char '\t'
  maxE <- double <* char '\t'
  sta <- decimal <* char '\t'
  miRL <- decimal <* char '\t'
  utrL <- decimal <* char '\t' <* skipSpace
  ps <- decimal `sepBy1` char ' '
  return $ MRecord miRId utrId ss sta miRL utrL

parseRecords :: Parser [MRecord]
parseRecords = parseRecord `sepBy1` endOfLine
