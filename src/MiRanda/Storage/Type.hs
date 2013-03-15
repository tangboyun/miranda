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

module MiRanda.Storage.Type where

import MiRanda.Types (Gene(..)
                     ,UTR(..)
                     ,MScore(..)
                     ,Pair(..)
                     ,Conservation(..)
                     ,RawScore(..)
                     ,ContextScore(..)
                     ,ContextScorePlus(..)
                     ,SeedType(..)
                     ,Align(..) 
                     )
import Data.ByteString (ByteString)

data Expression = Coding
                | NonCoding
                  deriving (Eq,Show)
                           
data GeneRecord = GR
  { geneInfo :: GeneInfo
  , mirSites :: [MiRSites]
  } deriving (Show,Eq)

data GeneInfo = GI
  { gene :: !Gene
  , expressionStyle :: !Expression
  , thisSpecies :: UTR
  , otherSpecies :: [UTR]
  } deriving (Show,Eq)

data MiRSites = MiRSites
  { mir :: !MiRNA
  , sites :: [Site]
  } deriving (Show,Eq)
             
data Site = Site
  { miRandaScore :: !MScore
  , conserveScore :: !Conservation
  , rawScore :: !RawScore
  , contextScore :: !(Maybe ContextScore)
  , contextScorePlus :: !(Maybe ContextScorePlus)
  , seedRange :: !Pair
  , siteRange :: !Pair
  , seed :: !SeedType
  , alignStructure :: !Align
  } deriving (Show,Eq)

data MiRNA = MiRNA
  { identity :: !ByteString -- "product" in miRBase feature
  , accession :: !ByteString -- miRBase accession id
  , isExperimentalValidated :: !Bool
  , seqdata :: !ByteString
  } deriving (Show,Eq)


