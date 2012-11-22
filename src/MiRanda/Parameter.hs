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
  [(8364, Latin "Xenopus" "tropicalis" "Frog") -- Western clawed frog
  ,(9031, Latin "Gallus" "gallus" "Chicken")
  ,(9258, Latin "Ornithorhynchus" "anatinus" "Platypus")
  ,(9361, Latin "Dasypus" "novemcinctus" "Armadillo") -- Nine-banded armadillo
  ,(9365, Latin "Erinaceus" "europaeus" "Hedgehog") -- Western European hedgehog
  ,(9371, Latin "Echinops" "telfairi" "Tenrec") -- Small Madagascar hedgehog, The lesser hedgehog tenrec
  ,(9544, Latin "Macaca" "mulatta" "Rhesus") -- Rhesus monkey
  ,(9598, Latin "Pan" "troglodytes" "Chimpanzee")
  ,(9606, Latin "Homo" "sapiens" "Human")
  ,(9615, Latin "Canis" "lupus" "Dog")
  ,(9685, Latin "Felis" "catus" "Cat") -- Domestic cat
  ,(9785, Latin "Loxodonta" "africana" "Elephant") -- African savanna elephant
  ,(9796, Latin "Equus" "caballus" "Horse")
  ,(9913, Latin "Bos" "taurus" "Cow") -- Cattle
  ,(9986, Latin "Oryctolagus" "cuniculus" "Rabbit")
  ,(10090, Latin "Mus" "musculus" "Mouse") -- House mouse
  ,(10116, Latin "Rattus" "norvegicus" "Rat") -- Norway rat
  ,(10141, Latin "Cavia" "porcellus" "Guinea pig") -- Domestic guinea pig
  ,(13616, Latin "Monodelphis" "domestica" "Opossum") -- Gray short-tailed opossum
  ,(28377, Latin "Anolis" "carolinensis" "Lizard") -- Green anole
  ,(30611, Latin "Otolemur" "garnettii" "Bushbaby") -- Small-eared galago
  ,(37347, Latin "Tupaia" "belangeri" "Tree Shrew") -- Northern tree shrew 树鼩 (乙肝模式动物)
  ,(42254, Latin "Sorex" "araneus" "Shrew")] -- European shrew
  