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
import Control.DeepSeq


data Expression = Coding
                | NonCoding
                  deriving (Eq,Show)
                           
data GeneRecord = GR
  { geneInfo :: !GeneInfo
  , mirSites :: ![MiRSites]
  } deriving (Show,Eq)

data GeneInfo = GI
  { gene :: !Gene
  , expressionStyle :: !Expression
  , thisSpecies :: !UTR
  , otherSpecies :: ![UTR]
  } deriving (Show,Eq)

data MiRSites = MiRSites
  { mir :: !MiRNA
  , sites :: ![Site]
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


instance NFData GeneRecord where
    rnf (GR gi mS) = rnf gi `seq` rnf mS `seq` ()
    
instance NFData GeneInfo where
    rnf (GI a b c ds) = rnf a `seq` rnf b `seq` rnf c `seq` rnf ds `seq` ()
    
instance NFData Expression where
    rnf a = a `seq` ()

instance NFData MiRNA where
    rnf (MiRNA a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` ()

instance NFData Site where
    rnf (Site a b c d e f g h i) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
        rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` ()

instance NFData MiRSites where
    rnf (MiRSites a b) = rnf a `seq` rnf b `seq` ()
    
        
