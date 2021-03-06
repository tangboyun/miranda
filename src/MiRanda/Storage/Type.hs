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
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Int

data Expression = Coding
                | NonCoding
                  deriving (Eq,Show)
                           
data GeneRecord = GR
  { geneInfo :: GeneInfo
  , mirSites :: [MiRSites]
  } deriving (Show,Eq)

data GeneInfo = GI
  { gene :: Gene
  , expressionStyle :: Expression
  , thisSpecies :: UTR
  , otherSpecies :: [UTR]
  } deriving (Show,Eq)

data MiRSites = MiRSites
  { mir :: MiRNA
  , sites :: [Site]
  } deriving (Show,Eq)
             
data Site = Site
  { miRandaScore :: MScore
  , conserveScore :: Conservation
  , rawScore :: RawScore
  , contextScore :: Maybe ContextScore
  , contextScorePlus :: Maybe ContextScorePlus
  , seedRange :: Pair
  , siteRange :: Pair
  , seed :: SeedType
  , alignStructure :: Align
  } deriving (Show,Eq)

data MiRNA = MiRNA
  { identity :: ByteString -- "product" in miRBase feature
  , accession :: ByteString -- miRBase accession id
  , isExperimentalValidated :: Bool
  , family :: Maybe Family
  , seqdata :: ByteString
  } deriving (Show,Eq)

data Family = Family
  { rFamID :: ByteString
  , miRBaseID :: ByteString
  , miRNAFamily :: ByteString
  } deriving (Show,Eq)

data EnDB = EnDB
  { seqnames :: V.Vector ByteString
  , genesymbols :: V.Vector ByteString
  , identities :: V.Vector ByteString
  , accessions :: V.Vector ByteString
--  , datum :: [(UV.Vector Int,UV.Vector Double)]
  , datContext :: UV.Vector Float
  , datSite :: UV.Vector Int16
  } deriving (Eq)

instance NFData GeneRecord where
    rnf (GR gi mS) = rnf gi `seq` rnf mS `seq` ()
    
instance NFData GeneInfo where
    rnf (GI a b c ds) = rnf a `seq` rnf b `seq` rnf c `seq` rnf ds `seq` ()
    
instance NFData Expression where
    rnf a = a `seq` ()

instance NFData Family where
    rnf (Family a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()
    
instance NFData MiRNA where
    rnf (MiRNA a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` ()

instance NFData Site where
    rnf (Site a b c d e f g h i) =
        rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq`
        rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` ()

instance NFData MiRSites where
    rnf (MiRSites a b) = rnf a `seq` rnf b `seq` ()
    
        
