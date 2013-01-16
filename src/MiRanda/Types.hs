{-# LANGUAGE GADTs, DisambiguateRecordFields #-}
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

module MiRanda.Types where

import Data.ByteString (ByteString)
import Data.Vector.Unboxed


data SiteLine = SL
  { miRID :: ByteString
  , geneID :: Gene
  , conserve_Score :: Conservation
  , raw_Score :: RawScore
  , context_Score :: Maybe ContextScore
  , context_Score_Plus :: Maybe ContextScorePlus
  , seedRange :: Pair
  , siteRange :: Pair
  , seed :: SeedType
  , alignStructure :: Align
  } deriving (Show,Eq)

newtype Inclusive = Inclusive (Int,Int)
                    deriving (Show,Eq)
data Gene = Gene 
  {syb :: ByteString
  ,ref :: ByteString
  } deriving (Show,Eq)
      
data Conservation = Con
  { isConserved :: !Bool
  , branchLength :: {-# UNPACK #-} !Double
  , pct :: !(Maybe Double)
  } deriving (Show,Eq)
  

data Fasta = Fasta
  { seqlabel :: ByteString
  , seqdata :: SeqData
  } deriving (Show,Eq)

data UTR = UTR
  {geneSymbol :: ByteString
  ,refSeqID :: ByteString
  ,taxonomyID :: Int
  ,alignment :: GapSeq
  } deriving (Show,Eq)

newtype GapSeq = GS
  {unGS :: ByteString
  } deriving (Show,Eq)

newtype SeqData = SD
  { unSD :: ByteString }
   deriving (Show,Eq)

data Record = Record
  {miRNA :: ByteString
  ,gene :: ByteString -- refSeqID
  ,utr :: UTR -- ref utr
  ,homoUTRs :: [UTR] -- utrs except for ref utr
  ,predictedSites :: [Site]
  } deriving (Show,Eq)

data Site = Site
  { miRandaScore :: MScore
  , rawScore :: RawScore
  , contextScore :: Maybe ContextScore
  , contextScorePlus :: Maybe ContextScorePlus
  , seedMatchRange :: Pair
  , utrRange :: Pair
  , match :: Match
  , seedType :: SeedType
  , align :: Align
  } deriving (Show,Eq)
   
data MRecord = MRecord
  { _miRNA :: ByteString
  , _mRNA :: ByteString -- refSeqID
  , sites :: [MSite]
  , strand :: {-# UNPACK #-} !Int
  , miRNALen :: {-# UNPACK #-} !Int
  , mRNALen :: {-# UNPACK #-} !Int    
  } deriving (Show)

data MSite = MSite
  { _mScore :: MScore 
  , _miRNARange :: Pair
  , _mRNARange :: Pair
  , _alignLen :: {-# UNPACK #-} !Int
  , _match :: Match
  , _seedType :: SeedType
  , _align :: Align
  } deriving (Show,Eq)

data Latin = Latin
  { genus :: ByteString
  , species :: ByteString
  , commonName :: ByteString
  } deriving (Eq)

data Setting = Setting
  { gapOpenPenalty :: {-# UNPACK #-} !Double
  , gapExtendPenalty :: {-# UNPACK #-} !Double
  , scoreThreshold :: {-# UNPACK #-} !Double
  , energyThreshold :: {-# UNPACK #-} !Double
  , scalingParameter :: {-# UNPACK #-} !Double
  } deriving (Show,Eq)

data Pair = P
  { beg :: {-# UNPACK #-} !Int -- begin at 0
  , end :: {-# UNPACK #-} !Int -- not include end
  } deriving (Show,Eq)

data MScore = MScore
  { structureScore :: {-# UNPACK #-} !Double
  , freeEnergy :: {-# UNPACK #-} !Double
  } deriving (Show,Eq,Ord)
             

data Match = Match
  { wasonClick :: {-# UNPACK #-} !Int
  , includeGU :: {-# UNPACK #-} !Int
  } deriving (Show,Eq)

data Align = Align
  { miRNASite3' :: ByteString
  , mRNASite5' :: ByteString
  , hydrogenBond :: ByteString
  } deriving (Show,Eq)

data Sta where
  A :: { per :: Double} -> Sta
  T :: { per :: Double} -> Sta
  G :: { per :: Double} -> Sta
  C :: { per :: Double} -> Sta
  U :: { per :: Double} -> Sta
  deriving (Show,Eq)

data SeedType = M8   -- ^ 8mer site
              | M7M8 -- ^ 7mer-m8 site
              | M7A1 -- ^ 7mer-A1 site
              | M6   -- ^ 6mer site                
              | M6O  -- ^ Offset 6mer site
              | Imperfect -- ^ imperfect seed site
              deriving (Eq,Ord,Enum)

data ContextScore = CS
  {context :: {-# UNPACK #-} !Double
  ,pairingContrib :: {-# UNPACK #-} !Double
  ,localAUContrib :: {-# UNPACK #-} !Double
  ,positionContrib :: {-# UNPACK #-} !Double
  ,siteTypeContrib :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord,Show)

data ContextScorePlus = CSP
  {contextPlus :: {-# UNPACK #-} !Double
  ,pairingContribPlus :: {-# UNPACK #-} !Double
  ,localAUContribPlus :: {-# UNPACK #-} !Double
  ,positionContribPlus :: {-# UNPACK #-} !Double
  ,taContribPlus :: {-# UNPACK #-} !Double
  ,spsContribPlus :: {-# UNPACK #-} !Double
  ,siteTypeContribPlus :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord,Show)
             
data Coef = Coef
  {slope :: {-# UNPACK #-} !Double
  ,intercept :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord)
   
data RawScore = RS
  {pairingScore :: PairScore
  ,auScore :: AUScore
  ,posScore :: PosScore
  } deriving (Show,Eq,Ord)


newtype PairScore = PairScore Double
                    deriving (Show,Eq,Ord)
newtype AUScore = AUScore Double
                  deriving (Show,Eq,Ord)

newtype PosScore = PosScore Double
                   deriving (Show,Eq,Ord)

newtype SPScore = SPScore Double
                   deriving (Show,Eq,Ord)
                            
newtype TAScore = TAScore Double
                   deriving (Show,Eq,Ord)
                            
instance Show SeedType where
  show s =
    case s of
      M8        -> "8mer"
      M7M8      -> "7mer-m8"
      M7A1      -> "7mer-A1"
      M6        -> "6mer"
      M6O       -> "Offset 6mer"
      Imperfect -> "Imperfect"
