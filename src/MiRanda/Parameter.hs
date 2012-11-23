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


-- | 6mer SPS value for Conserved vertebrate miRNAs
spsMapmer6 :: IntMap Double
spsMapmer6 = IM.fromList
             [(8882,-6.72)
             ,(10299,-5.66)
             ,(10273,-6.43)
             ,(8171,-5.22)
             ,(1403,-8.41)
             ,(2340,-8.57)
             ,(185,-3.75)
             ,(697,-6.08)
             ,(11840,-6.27)
             ,(2547,-4.85)
             ,(2533,-8.57)
             ,(13391,-4.92)
             ,(10706,-8.68)
             ,(3985,-4.68)
             ,(13356,-4.72)
             ,(13387,-6.35)
             ,(2324,-8.57)
             ,(11268,-3.58)
             ,(10504,-7.71)
             ,(14654,-4.68)
             ,(10542,-8.57)
             ,(16017,-6.84)
             ,(1388,-8.37)
             ,(1201,-5.01)
             ,(9363,-8.60)
             ,(10427,-7.95)
             ,(657,-6.81)
             ,(5602,-8.52)
             ,(6934,-7.34)
             ,(4398,-6.24)
             ,(16377,-1.29)
             ,(2960,-6.97)
             ,(16053,-6.35)
             ,(3743,-6.98)
             ,(13287,-4.35)
             ,(10158,-8.57)
             ,(7243,-5.75)
             ,(2991,-6.94)
             ,(286,-4.61)
             ,(11451,-5.91)
             ,(8760,-5.90)
             ,(1203,-5.01)
             ,(13615,-7.05)
             ,(8711,-5.86)
             ,(4836,-7.87)
             ,(7504,-8.52)
             ,(14642,-5.08)
             ,(12519,-4.35)
             ,(1268,-4.19)
             ,(16016,-6.84)
             ,(3729,-7.01)
             ,(10344,-9.38)
             ,(7095,-7.21)
             ,(9019,-3.90)
             ,(416,-6.15)
             ,(14428,-7.05)
             ,(1957,-8.57)
             ,(11282,-4.31)
             ,(2763,-6.45)
             ,(5307,-7.71)
             ,(798,-2.69)
             ,(14350,-2.43)
             ,(13695,-6.89)
             ,(6099,-6.19)
             ,(12422,-4.39)
             ,(15259,-7.70)
             ,(301,-4.61)
             ,(4682,-7.71)
             ,(884,-4.27)
             ,(887,-4.27)
             ,(1939,-6.97)
             ,(15263,-6.97)
             ,(9197,-4.19)
             ,(10003,-6.64)
             ,(11567,-6.48)
             ,(743,-6.24)
             ,(5267,-8.44)
             ,(917,-6.28)
             ,(16118,-4.02)
             ,(9032,-5.90)
             ,(3601,-4.92)
             ,(367,-6.04)
             ,(3813,-6.44)
             ,(378,-6.08)
             ,(12423,-4.39)
             ,(2342,-8.57)
             ,(6228,-9.38)]
