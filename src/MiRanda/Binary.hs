-----------------------------------------------------------------------------
-- |
-- Module : ghc 7.6.2 can not derivng Binary instance automatically ...
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.Binary where

import MiRanda.Types
import qualified MiRanda.Storage.Type as ST
import MiRanda.Storage.Type hiding (Site(..))
import Data.Binary
import Control.Applicative
import Data.Word
import qualified Codec.Compression.Zlib as Z
import Data.ByteString.Lazy (ByteString)


compress :: Binary a => a -> ByteString
{-# INLINE compress #-}
compress v = Z.compressWith
             Z.defaultCompressParams { Z.compressLevel = Z.bestCompression } $
             encode v
             
decompress :: Binary a => ByteString -> a
{-# INLINE decompress #-}
decompress b = decode $ 
               Z.decompressWith Z.defaultDecompressParams b
               
instance Binary Pair where
    put (P b e) = do
        put b
        put e
    get = P <$> get <*> get

instance Binary UTR where
    put (UTR g r t a) = do
        put g
        put r
        put t
        put a
    get = UTR <$> get <*> get <*> get <*> get

instance Binary SeedType where
    put s =
        case s of
            M8 -> put (0 :: Word8)
            M7M8 -> put (1 :: Word8)
            M7A1 -> put (2 :: Word8)
            M6 -> put (3 :: Word8)
            M6O -> put (4 :: Word8)
            Imperfect -> put (5 :: Word8)
    get = do
        t <- get :: Get Word8
        case t of
            0 -> return M8
            1 -> return M7M8
            2 -> return M7A1
            3 -> return M6
            4 -> return M6O
            5 -> return Imperfect

instance Binary RawScore where
    put (RS a b c) = do
        put a
        put b
        put c
    get = RS <$> get <*> get <*> get

instance Binary MScore where
    put (MScore a b) = do
        put a
        put b
    get = MScore <$> get <*> get

instance Binary ContextScore where
    put (CS a b c d e) = do
        put a
        put b
        put c
        put d
        put e
    get = CS <$> get <*> get <*> get <*>
          get <*> get

instance Binary ContextScorePlus where
    put (CSP a b c d e f g) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g
    get = CSP <$> get <*> get <*> get <*>
          get <*> get <*> get <*> get

instance Binary Match where
    put (Match a b ) = do
        put a
        put b
    get = Match <$> get <*> get

instance Binary Align where
    put (Align a b c) = do
        put a
        put b
        put c
    get = Align <$> get <*> get <*> get

instance Binary Site where
    put (Site a b c d e f g h i) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g
        put h
        put i
    get = Site <$> get <*> get <*> get <*>
          get <*> get <*> get <*> get <*>
          get <*> get

instance Binary Record where
    put (Record a b c d e) = do
        put a
        put b
        put c
        put d
        put e
    get = Record <$> get <*> get <*> get <*>
          get <*> get

instance Binary Conservation where
    put (Con a b c) = do
        put a
        put b
        put c
    get = Con <$> get <*> get <*> get

instance Binary ST.Site where
    put (ST.Site a b c d e f g h i) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g
        put h
        put i
    get = ST.Site <$> get <*> get <*> get <*>
          get <*> get <*> get <*> get <*> get <*>
          get

instance Binary MiRNA where
    put (MiRNA a b c d) = do
        put a
        put b
        put c
        put d
    get = MiRNA <$> get <*> get <*> get <*> get
    
instance Binary MiRSites where
    put (MiRSites a b) = do
        put a
        put b
    get = MiRSites <$> get <*> get

instance Binary Expression where
    put e =
        case e of
            Coding -> put (0 :: Word8)
            NonCoding -> put (1 :: Word8)
    get = do
        e <- get :: Get Word8
        case e of
            0 -> return Coding
            1 -> return NonCoding

instance Binary Gene where
    put (Gene a b) = do
        put a
        put b
    get = Gene <$> get <*> get
        
instance Binary GeneInfo where
    put (GI a b c d) = do
        put a
        put b
        put c
        put d
    get = GI <$> get <*> get <*> get <*> get
    
instance Binary GeneRecord where
    put (GR a b) = do
        put a
        put b
    get = GR <$> get <*> get
