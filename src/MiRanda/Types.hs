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

data Record = Record
  { para :: Setting
  , miRNA :: ByteString
  , mRNA :: ByteString
  , sites :: [Site]
  , statistics :: Stat
  } deriving (Show)

data Setting = Setting
  { gapOpenPenalty :: {-# UNPACK #-} !Double
  , gapExtendPenalty :: {-# UNPACK #-} !Double
  , scoreThreshold :: {-# UNPACK #-} !Double
  , energyThreshold :: {-# UNPACK #-} !Double
  , scalingParameter :: {-# UNPACK #-} !Double
  } deriving (Eq)

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
  { beg :: {-# UNPACK #-} !Int
  , end :: {-# UNPACK #-} !Int
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
  }

data Identity = Id
  { idPerfectMatch :: {-# UNPACK #-} !Double
  , idIncludeGU :: {-# UNPACK #-} !Double
  } deriving (Show,Eq)

data Align = Align
  { miRNASite3' :: ByteString
  , mRNASite5' :: ByteString
  , hydrogenBond :: ByteString
  } deriving (Show,Eq)
             
data SeedType = M8   -- ^ 8mer site
              | M7M8 -- ^ 7mer-m8 site
              | M7A1 -- ^ 7mer-A1 site
              | M6   -- ^ 6mer site                
              | M6O  -- ^ Offset 6mer site
              | M8GU -- ^ 8mer GU wobble
              | M7GU -- ^ 7mer GU wobble
              | M8BM -- ^ 8mer with bulge in site
              | M7BI -- ^ 7mer with bulge in miRNA
              | M8Mis -- ^ 8mer with mismatch
              | M7Mis -- ^ 7mer with mismatch
              deriving (Eq,Ord)

instance Show SeedType where
  show s =
    case s of
      M8    -> "8mer site"
      M7M8  -> "7mer-m8 site"
      M7A1  -> "7mer-A1 site"
      M6    -> "6mer site"
      M6O   -> "Offset 6mer site"
      M8GU  -> "8mer GU wobble"
      M7GU  -> "7mer GU wobble"
      M8BM  -> "8mer with bulge in site"
      M7BI  -> "7mer with bulge in miRNA"
      M8Mis -> "8mer with mismatch"
      M7Mis -> "7mer with mismatch"
