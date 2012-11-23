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

siteContribMap :: IntMap (Maybe Double)
siteContribMap = IM.fromList
                 [(fromEnum M8, Just (-0.31))
                 ,(fromEnum M7M8, Just (-0.161))
                 ,(fromEnum M7A1, Just (-0.099))
                 ,(fromEnum M6, Just (-0.015))
                 ,(fromEnum M6O, Just (-0.015))
                 ,(fromEnum Imperfect, Nothing)]

paCoefMap :: IntMap (Maybe Coef)
paCoefMap = IM.fromList
            [(fromEnum M8, Just (Coef (-0.0041) (-0.299)))
            ,(fromEnum M7M8, Just (Coef (-0.031) (-0.094)))
            ,(fromEnum M7A1, Just (Coef (-0.0211) (-0.0211)))
            ,(fromEnum M6, Just (Coef (-0.00278) (-0.0091)))
            ,(fromEnum M6O, Just (Coef (-0.00278) (-0.0091)))
            ,(fromEnum Imperfect, Nothing)]

auCoefMap :: IntMap (Maybe Coef)
auCoefMap = IM.fromList
            [(fromEnum M8, Just (Coef (-0.64) 0.055))
            ,(fromEnum M7M8, Just (Coef (-0.50) 0.108))
            ,(fromEnum M7A1, Just (Coef (-0.42) 0.137))
            ,(fromEnum M6, Just (Coef (-0.241) 0.115))
            ,(fromEnum M6O, Just (Coef (-0.241) 0.115))
            ,(fromEnum Imperfect, Nothing)]

psCoefMap :: IntMap (Maybe Coef)
psCoefMap = IM.fromList
            [(fromEnum M8, Just (Coef 0.000172 (-0.38)))
            ,(fromEnum M7M8, Just (Coef 0.000091 (-0.198)))
            ,(fromEnum M7A1, Just (Coef 0.000072 (-0.131)))
            ,(fromEnum M6, Just (Coef 0.000049 (-0.033)))
            ,(fromEnum M6O, Just (Coef 0.000049 (-0.033)))
            ,(fromEnum Imperfect, Nothing)]
            
