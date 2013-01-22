{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}
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
import Data.Monoid

data RefLine = RL
  { rmiRID :: !ByteString
  , rgene :: !Gene
  , rmirandaScore :: !MScore
  , rconserveScore :: !Conservation
  , rrawScore :: !RawScore
  , rcontextScore :: !(Maybe ContextScore)
  , rcontextScorePlus :: !(Maybe ContextScorePlus)
  , totalSite :: !Int
  , conservedSite :: !(Int,Int,Int,Int,Int,Int)
  , nonConservedSite :: !(Int,Int,Int,Int,Int,Int)
  , utrLength :: !Int
  , utrSeq :: !ByteString
  } deriving (Show,Eq)

data SiteLine = SL
  { miRID :: ByteString
  , geneID :: Gene
  , mirandaScore :: MScore
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
  } deriving (Show,Eq,Ord)

data MScore = MScore
  { structureScore :: {-# UNPACK #-} !Double
  , freeEnergy :: {-# UNPACK #-} !Double
  } deriving (Show,Eq)
             

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
                    deriving (Show,Eq,Ord,Additive)
newtype AUScore = AUScore Double
                  deriving (Show,Eq,Ord,Additive)

newtype PosScore = PosScore Double
                   deriving (Show,Eq,Ord,Additive)

newtype SPScore = SPScore Double
                   deriving (Show,Eq,Ord,Additive)
                            
newtype TAScore = TAScore Double
                   deriving (Show,Eq,Ord,Additive)
                            
instance Show SeedType where
  show s =
    case s of
      M8        -> "8mer"
      M7M8      -> "7mer-m8"
      M7A1      -> "7mer-A1"
      M6        -> "6mer"
      M6O       -> "Offset 6mer"
      Imperfect -> "Imperfect"


class Additive a where
    add :: a -> a -> a

instance Additive Bool where
    add False False = False
    add _ _ = True
    
instance Additive Double where
    add = (+)

instance Additive Int where
    add = (+)

instance Additive a => Additive (Maybe a) where
    add Nothing Nothing = Nothing
    add Nothing b@(Just _) = b
    add a@(Just _) Nothing = a
    add (Just a) (Just b) = Just $ a `add` b

instance Additive MScore where
    add (MScore a1 b1) (MScore a2 b2) =
        MScore (a1 `add` a2) (b1 `add` b2)

instance Additive RawScore where
    add (RS a1 b1 c1) (RS a2 b2 c2) =
        RS (a1 `add` a2) (b1 `add` b2) (c1 `add` c2)

instance Additive ContextScore where
    add (CS a1 b1 c1 d1 e1) (CS a2 b2 c2 d2 e2) =
        CS (a1 `add` a2) (b1 `add` b2) (c1 `add` c2)
        (d1 `add` d2) (e1 `add` e2)




instance Additive ContextScorePlus where
    add (CSP a1 b1 c1 d1 e1 f1 g1)
        (CSP a2 b2 c2 d2 e2 f2 g2) =
        CSP (a1 `add` a2) (b1 `add` b2)
        (c1 `add` c2) (d1 `add` d2)
        (e1 `add` e2) (f1 `add` f2)
        (g1 `add` g2)

instance Additive Conservation where
    add (Con a1 b1 c1) (Con a2 b2 c2) =
        Con (a1 `add` a2) (b1 `add` b2) (c1 `add` c2)

instance Ord RefLine where
    compare rf1 rf2 =
        myCompare (rcontextScorePlus rf1) (rcontextScorePlus rf2) <>
        myCompare (rcontextScore rf1) (rcontextScore rf2) <>
        compare (rmirandaScore rf1) (rmirandaScore rf2) <>
        compare (rconserveScore rf1) (rconserveScore rf2) <>
        flip compare (totalSite rf1) (totalSite rf2)

myCompare :: Ord a => Maybe a -> Maybe a -> Ordering
myCompare Nothing Nothing = EQ
myCompare (Just a1) (Just a2) = compare a1 a2
myCompare (Just _) Nothing = LT
myCompare Nothing (Just _) = GT
      
instance Ord MScore where
    compare ms1 ms2 =
        flip compare (structureScore ms1) (structureScore ms2) <>
        compare (freeEnergy ms1) (freeEnergy ms2)

instance Ord Conservation where
    compare con1 con2 =
        flip compare (branchLength con1) (branchLength con2) <>
        flip myCompare (pct con1) (pct con2)
        

