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

module MiRanda.Parameter where

import MiRanda.Types
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.ByteString (ByteString)

taxMap :: IntMap Latin
taxMap =
  IM.fromList
  [(8364, Latin "Xenopus" "tropicalis" "Western clawed frog")
  ,(9031, Latin "Gallus" "gallus" "Chicken")
  ,(9258, Latin "Ornithorhynchus" "anatinus" "Platypus")
  ,(9361, Latin "Dasypus" "novemcinctus" "Nine-banded armadillo")
  ,(9365, Latin "Erinaceus" "europaeus" "Western European hedgehog")
  ,(9371, Latin "Echinops" "telfairi" "Small Madagascar hedgehog")
  ,(9544, Latin "Macaca" "mulatta" "Rhesus monkey")
  ,(9598, Latin "Pan" "troglodytes" "Chimpanzee")
  ,(9606, Latin "Homo" "sapiens" "Human")
  ,(9615, Latin "Canis" "lupus" "Dog")
  ,(9685, Latin "Felis" "catus" "Domestic cat")
  ,(9785, Latin "Loxodonta" "africana" "African savanna elephant")
  ,(9796, Latin "Equus" "caballus" "Horse")
  ,(9913, Latin "Bos" "taurus" "Cattle")
  ,(9986, Latin "Oryctolagus" "cuniculus" "Rabbit")
  ,(10090, Latin "Mus" "musculus" "House mouse")
  ,(10116, Latin "Rattus" "norvegicus" "Norway rat")
  ,(10141, Latin "Cavia" "porcellus" "Domestic guinea pig")
  ,(13616, Latin "Monodelphis" "domestica" "Gray short-tailed opossum")
  ,(28377, Latin "Anolis" "carolinensis" "Green anole")
  ,(30611, Latin "Otolemur" "garnettii" "Small-eared galago")
  ,(37347, Latin "Tupaia" "belangeri" "Northern tree shrew")
  ,(42254, Latin "Sorex" "araneus" "European shrew")]
  
