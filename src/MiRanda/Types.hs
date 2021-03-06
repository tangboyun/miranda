{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
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
import Data.Monoid
import Data.Binary
import Control.DeepSeq


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
  { miRID :: !ByteString
  , geneID :: !Gene
  , mirandaScore :: !MScore
  , conserve_Score :: !Conservation
  , raw_Score :: !RawScore
  , context_Score :: !(Maybe ContextScore)
  , context_Score_Plus :: !(Maybe ContextScorePlus)
  , seedRange :: !Pair
  , siteRange :: !Pair
  , seed :: !SeedType
  , alignStructure :: !Align
  } deriving (Show,Eq)

data Gene = Gene 
  {syb :: !ByteString
  ,ref :: !ByteString -- for lnc, this is SeqID (Seqname)
                     -- for mRNA, this is RefSeq
  } deriving (Show,Eq)
      
data Conservation = Con
  { isConserved :: !Bool
  , branchLength :: {-# UNPACK #-} !Double
  , pct :: !(Maybe Double)
  } deriving (Show,Eq)
  

data Fasta = Fasta
  { seqlabel :: !ByteString
  , seqdata :: !SeqData
  } deriving (Show,Eq)

data UTR = UTR
  {geneSymbol :: !ByteString
  ,refSeqID :: !ByteString
  ,taxonomyID :: !Int
  ,alignment :: !GapSeq
  } deriving (Show,Eq)

newtype GapSeq = GS
  {unGS :: ByteString
  } deriving (Show,Eq,Binary)

newtype SeqData = SD
  { unSD :: ByteString }
   deriving (Show,Eq)

data Record = Record
  {miRNA :: !ByteString
  ,gene :: !ByteString -- refSeqID
  ,utr :: !UTR -- ref utr
  ,homoUTRs :: [UTR] -- utrs except for ref utr
  ,predictedSites :: [Site]
  } deriving (Show,Eq)

data Site = Site
  { miRandaScore :: !MScore
  , rawScore :: !RawScore
  , contextScore :: !(Maybe ContextScore)
  , contextScorePlus :: !(Maybe ContextScorePlus)
  , seedMatchRange :: !Pair
  , utrRange :: !Pair
  , match :: !Match
  , seedType :: !SeedType
  , align :: !Align
  } deriving (Show,Eq)
   
data MRecord = MRecord
  { _miRNA :: !ByteString
  , _mRNA :: !ByteString -- refSeqID
  , sites :: [MSite]
  , strand :: !Int
  , miRNALen :: !Int
  , mRNALen :: !Int    
  } deriving (Show)

data MSite = MSite
  { _mScore :: !MScore 
  , _miRNARange :: !Pair
  , _mRNARange :: !Pair
  , _alignLen :: !Int
  , _match :: !Match
  , _seedType :: !SeedType
  , _align :: !Align
  } deriving (Show,Eq)

data Latin = Latin
  { genus :: !ByteString
  , species :: !ByteString
  , commonName :: !ByteString
  } deriving (Eq)

data Setting = Setting
  { gapOpenPenalty :: !Double
  , gapExtendPenalty :: !Double
  , scoreThreshold :: !Double
  , energyThreshold :: !Double
  , scalingParameter :: !Double
  } deriving (Show,Eq)

data Pair = P
  { beg :: !Int -- begin at 0
  , end :: !Int -- not include end
  } deriving (Show,Eq,Ord)

data MScore = MScore
  { structureScore :: !Double
  , freeEnergy :: !Double
  } deriving (Show,Eq)
             

data Match = Match
  { wasonClick :: !Int
  , includeGU :: !Int
  } deriving (Show,Eq)

data Align = Align
  { miRNASite3' :: !ByteString
  , mRNASite5' :: !ByteString
  , hydrogenBond :: !ByteString
  } deriving (Show,Eq)

data Sta where
  A :: { per :: !Double} -> Sta
  T :: { per :: !Double} -> Sta
  G :: { per :: !Double} -> Sta
  C :: { per :: !Double} -> Sta
  U :: { per :: !Double} -> Sta
  deriving (Show,Eq)

data SeedType = M8   -- ^ 8mer site
              | M7M8 -- ^ 7mer-m8 site
              | M7A1 -- ^ 7mer-A1 site
              | M6   -- ^ 6mer site                
              | M6O  -- ^ Offset 6mer site
              | Imperfect -- ^ imperfect seed site
              deriving (Eq,Ord,Enum)

data ContextScore = CS
  {context :: !Double
  ,pairingContrib :: !Double
  ,localAUContrib :: !Double
  ,positionContrib :: !Double
  ,siteTypeContrib :: !Double
  } deriving (Eq,Ord,Show)

data ContextScorePlus = CSP
  {contextPlus :: !Double
  ,pairingContribPlus ::  !Double
  ,localAUContribPlus ::  !Double
  ,positionContribPlus ::  !Double
  ,taContribPlus ::  !Double
  ,spsContribPlus ::  !Double
  ,siteTypeContribPlus ::  !Double
  } deriving (Eq,Ord,Show)
             
data Coef = Coef
  {slope ::  !Double
  ,intercept ::  !Double
  } deriving (Eq,Ord)
   
data RawScore = RS
  {pairingScore :: !PairScore
  ,auScore :: !AUScore
  ,posScore :: !PosScore
  } deriving (Show,Eq,Ord)


newtype PairScore = PairScore
  {unPair :: Double
  } deriving (Show,Eq,Ord,Additive,Binary)
             
newtype AUScore = AUScore
  {unAu :: Double
  } deriving (Show,Eq,Ord,Additive,Binary)

newtype PosScore = PosScore
  {unPos :: Double
  } deriving (Show,Eq,Ord,Additive,Binary)

newtype SPScore = SPScore Double
                   deriving (Show,Eq,Ord,Additive,Binary)
                            
newtype TAScore = TAScore Double
                   deriving (Show,Eq,Ord,Additive,Binary)


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
    {-# INLINE add #-}
    
instance Additive Double where
    add = (+)
    {-# INLINE add #-}

instance Additive Int where
    add = (+)
    {-# INLINE add #-}

instance Additive a => Additive (Maybe a) where
    add Nothing Nothing = Nothing
    add Nothing b@(Just _) = b
    add a@(Just _) Nothing = a
    add (Just a) (Just b) = Just $! a `add` b
    {-# INLINE add #-}

instance Additive MScore where
    add (MScore !a1 !b1) !(MScore !a2 !b2) =
        let !a = a1 `add` a2
            !b = b1 `add` b2
        in MScore a b
    {-# INLINE add #-}

instance Additive RawScore where
    add (RS !a1 !b1 !c1) (RS !a2 !b2 !c2) =
        let !a = a1 `add` a2
            !b = b1 `add` b2
            !c = c1 `add` c2
        in RS a b c
    {-# INLINE add #-}

instance Additive ContextScore where
    add (CS !a1 !b1 !c1 !d1 !e1) (CS !a2 !b2 !c2 !d2 !e2) =
        let !a = a1 `add` a2
            !b = b1 `add` b2
            !c = c1 `add` c2
            !d = d1 `add` d2
            !e = e1 `add` e2
        in CS a b c d e
    {-# INLINE add #-}



instance Additive ContextScorePlus where
    add (CSP !a1 !b1 !c1 !d1 !e1 !f1 !g1)
        (CSP !a2 !b2 !c2 !d2 !e2 !f2 !g2) =
        let !a = a1 `add` a2
            !b = b1 `add` b2
            !c = c1 `add` c2
            !d = d1 `add` d2
            !e = e1 `add` e2
            !f = f1 `add` f2
            !g = g1 `add` g2
        in CSP a b c d e f g
    {-# INLINE add #-}

instance Additive Conservation where
    add (Con !a1 !b1 !c1) (Con !a2 !b2 !c2) =
        let !a = a1 `add` a2
            !b = b1 `add` b2
            !c = addPct c1 c2
        in Con a b c
      where
        addPct (Just pct1) (Just pct2) = Just $ 1 - (1-pct1)*(1-pct2)
        addPct p1@(Just _) _ = p1
        addPct _ p2@(Just _) = p2
        addPct _ _ = Nothing
    {-# INLINE add #-}

instance Ord RefLine where
    compare !rf1 !rf2 =
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
{-# INLINE myCompare #-}

instance Ord MScore where
    compare ms1 ms2 =
        flip compare (structureScore ms1) (structureScore ms2) <>
        compare (freeEnergy ms1) (freeEnergy ms2)
    {-# INLINE compare #-}
    
instance Ord Conservation where
    compare con1 con2 =
        flip compare (branchLength con1) (branchLength con2) <>
        flip myCompare (pct con1) (pct con2)
    {-# INLINE compare #-}        

instance NFData Gene where
    rnf (Gene a b) = rnf a `seq` rnf b `seq` ()

instance NFData UTR where
    rnf (UTR a b c (GS d)) = rnf a `seq`
                             rnf b `seq`
                             rnf c `seq`
                             rnf d `seq` ()
                             
instance NFData MScore where
    rnf (MScore a b) = rnf a `seq` rnf b `seq` ()

instance NFData Conservation where
    rnf (Con a b c) = rnf a `seq` rnf b `seq` rnf c `seq` ()

instance NFData RawScore where
    rnf (RS (PairScore a) (AUScore b) (PosScore c)) =
        rnf a `seq` rnf b `seq` rnf c `seq` ()

instance NFData Pair where
    rnf (P a b) = rnf a `seq` rnf b `seq` ()

instance NFData SeedType where
    rnf s = s `seq` ()
    
instance NFData Align where
    rnf (Align a b c) =
        rnf a `seq` rnf b `seq` rnf c `seq` ()

instance NFData ContextScore where
    rnf (CS a b c d e) = rnf a `seq` rnf b `seq`
                         rnf c `seq` rnf d `seq`
                         rnf e `seq` ()

instance NFData ContextScorePlus where
    rnf (CSP a b c d e f g) = rnf a `seq` rnf b `seq`
                              rnf c `seq` rnf d `seq`
                              rnf e `seq` rnf f `seq`
                              rnf g `seq` ()
    
instance NFData RefLine where
    rnf (RL a b c d e f g h i j k l) = rnf a `seq` rnf b `seq`
                                       rnf c `seq` rnf d `seq`
                                       rnf e `seq` rnf f `seq`
                                       rnf g `seq` rnf h `seq`                                       
                                       rnf i `seq` rnf j `seq`
                                       rnf k `seq` rnf l `seq` ()

instance NFData SiteLine where
    rnf (SL a b c d e f g h i j k) = rnf a `seq` rnf b `seq`
                                     rnf c `seq` rnf d `seq`
                                     rnf e `seq` rnf f `seq`
                                     rnf g `seq` rnf h `seq`                                       
                                     rnf i `seq` rnf j `seq`
                                     rnf k `seq` ()
instance NFData Match where
    rnf (Match a b) = rnf a `seq` rnf b `seq` ()

instance NFData Site where
    rnf (Site a b c d e f g h i) = rnf a `seq` rnf b `seq`
                                   rnf c `seq` rnf d `seq`
                                   rnf e `seq` rnf f `seq`
                                   rnf g `seq` rnf h `seq`                                       
                                   rnf i `seq` ()

instance NFData Record where
    rnf (Record a b c d e) = rnf a `seq` rnf b `seq`
                             rnf c `seq` rnf d `seq`
                             rnf e `seq` ()
                             
