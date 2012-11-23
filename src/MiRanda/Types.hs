{-# LANGUAGE GADTs #-}
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



data TSP = TSP
  {spsM8orM7M8 :: {-# UNPACK #-} !Double
  ,pssM7A1 :: {-# UNPACK #-} !Double
  ,taValue :: {-# UNPACK #-} !Double
  } deriving (Show,Eq)
             
data UTR = UTR
  {geneSymbol :: ByteString
  ,taxonomyID :: Int
  ,alignment :: GapSeq
  } deriving (Show,Eq)

newtype GapSeq = GS ByteString
               deriving (Show,Eq)
newtype SeqData = SD ByteString
                deriving (Show,Eq)
                         
data Record = Record
  { para :: Setting
  , miRNA :: ByteString
  , mRNA :: ByteString
  , sites :: [Site]
  , statistics :: Stat
  } deriving (Show)

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

data Stat = Stat
  { totalScore :: {-# UNPACK #-} !Double
  , totalEnergy :: {-# UNPACK #-} !Double
  , maxScore :: {-# UNPACK #-} !Double
  , maxEnergy :: {-# UNPACK #-} !Double
  , strand :: ByteString
  , miRNALen :: {-# UNPACK #-} !Int
  , mRNALen :: {-# UNPACK #-} !Int
  , positions :: [Int]
  } deriving (Show,Eq)
             
data Pair = P
  { beg :: {-# UNPACK #-} !Int -- begin at 0
  , end :: {-# UNPACK #-} !Int -- include end
  } deriving (Show,Eq)
            
data Site = Site
  { score :: {-# UNPACK #-} !Double
  , energy :: {-# UNPACK #-} !Double
  , miRNARange :: Pair
  , mRNARange :: Pair
  , alignLen :: {-# UNPACK #-} !Int
  , identity :: Identity
  , seedType :: SeedType
  , align :: Align
  } deriving (Show,Eq)

data Identity = Id
  { idPerfectMatch :: {-# UNPACK #-} !Double
  , idIncludeGU :: {-# UNPACK #-} !Double
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
  {contextScore :: {-# UNPACK #-} !Double
  ,pairingContrib :: {-# UNPACK #-} !Double
  ,localAUContrib :: {-# UNPACK #-} !Double
  ,positionContrib :: {-# UNPACK #-} !Double
  ,siteTypeContrib :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord,Show)
   
data Coef = Coef
  {slope :: {-# UNPACK #-} !Double
  ,intercept :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord)
   
            

newtype PairScore = PairScore Double
                    deriving (Show,Eq)
newtype AUScore = AUScore Double
                  deriving (Show,Eq)

newtype PosScore = PosScore Double
                   deriving (Show,Eq)

instance Show SeedType where
  show s =
    case s of
      M8        -> "8mer"
      M7M8      -> "7mer-m8"
      M7A1      -> "7mer-A1"
      M6        -> "6mer"
      M6O       -> "Offset 6mer"
      Imperfect -> "Imperfect"
