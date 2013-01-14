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
import Data.List
import MiRanda.BranchLen.Newick
import MiRanda.Parameter.BL
import MiRanda.Types
import qualified Data.HashMap.Strict as H
import Control.Arrow
import qualified Data.ByteString.Char8 as B8
import Data.Function
import MiRanda.BranchLen
import System.Environment


main =
  fmap head getArgs >>= 
  B8.readFile >>=
  print .
  toBranchLength .
  map ((head *** id) . partition ((== 9606) . taxonomyID)) . groupBy ((==) `on` geneSymbol) .
  map ((\(a:b:c:[]) -> UTR a (read $ B8.unpack b) (GS c)).
       (B8.split '\t')) . B8.lines . B8.filter (/= '\r')
  
