{-# LANGUAGE OverloadedStrings #-}
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

module MiRanda.Util where

import MiRanda.Types
import qualified Data.ByteString.Char8 as B8
import Data.Char

parseSeedType :: Align -> SeedType
parseSeedType (Align miR3' mR5' b) = 
  let miR = B8.reverse miR3'
      nS  = B8.length seed
      seed = flip B8.take miR $
             (1 +) $ last $ take 8 $
             B8.findIndices isAlpha miR
      sVec = UV.fromList $ B8.unpack seed
      idx2 = UV.take 2 $ UV.findIndices isAlpha sVec
      fstI = UV.head idx2
      sndI = UV.last idx2
      site = B8.take nS $
             B8.reverse mR5'
      bond = B8.take nS $
             B8.reverse b
  in if B8.head bond == ' ' &&
        B8.last bond == ' '
     then M6
     else if B8.index bond fstI == ' ' &&
             B8.index bond sndI == ' '
          then M6O
          else if B8.head bond /= ' ' &&
                  B8.last bond /= ' '
               then go8 sVec site bond
               else go7or8 sVec site bond
  where 
    
