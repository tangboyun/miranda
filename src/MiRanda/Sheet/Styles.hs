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

module MiRanda.Sheet.Styles
       where
import Data.Colour.Names
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Types

defaultS = emptyStyle { fontName = Just "Times New Roman"
                      , fontFamily = Just "Roman"
                      , fontSize = Just 10
                      , hAlign = Just "Left"
                      }
boldCell = defaultS { fontIsBold = Just True }
title = boldCell { hAlign = Just "Center" }
  
summaryHeadStyle = defaultS
  { bgColor = Just khaki
  , vAlign = Just "Top"
  , hAlign = Just "Left"
  , wrapText = Just True
  , fontSize = Just 10
  }

headCell = title { bgColor = Just cornflowerblue }
tsScoreCell = title { bgColor = Just red }
miScoreCell = title { bgColor = Just green }
seedMatchCell = title { bgColor = Just mediumslateblue }
annoCell = title { bgColor = Just lightpink }
