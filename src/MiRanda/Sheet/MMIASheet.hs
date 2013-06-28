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

module MiRanda.Sheet.MMIASheet
       (
       )
       where
import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Char
import           Data.Colour.Names
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           MiRanda.Score
import           MiRanda.Sheet.Styles
import           MiRanda.Storage.Type
import           MiRanda.Types (Gene(..),ContextScorePlus(..),ContextScore(..),Conservation(..),SeedType(..),MScore(..),RawScore(..),Pair(..),PairScore(..),AUScore(..),PosScore(..))
import           MiRanda.Util
import           System.FilePath
import           Text.Printf
import           Text.XML.SpreadsheetML.Builder
import           Text.XML.SpreadsheetML.Types
import           Text.XML.SpreadsheetML.Util
import MiRanda.Sheet.Template
import Control.Arrow
import Data.Function


nameCells = [string "Identity"
            ,string "Accession"
            ,string "Fold Change - miRNA"
            ,string "Regulation - miRNA"
            ,string "Seqname"
            ,string "GeneSymbol"
            ,string "Fold Change - mRNA/LncRNA"
            ,string "Regulation - mRNA/LncRNA"
            ,string "Diagrams"
            ,string "Total"
            ,string "Context+"
            ,string "Context"
            ,string "Structure"
            ,string "Energy"
            ,string "Branch Length"
            ,string "Pct"
            ,string "8mer"
            ,string "7mer-m8"
            ,string "7mer-A1"
            ,string "6mer"
            ,string "Offset 6mer"
            ,string "Imperfect"
            ,string "8mer"
            ,string "7mer-m8"
            ,string "7mer-A1"
            ,string "6mer"
            ,string "Offset 6mer"
            ,string "Imperfect"
            ]
