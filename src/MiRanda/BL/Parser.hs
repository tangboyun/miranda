{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- Borrowed from the phybin <http://hackage.haskell.org/package/phybin> package
module Bio.Phylogeny.PhyBin.Parser
       (
         newick_parser
       , parseNewick
       )
       where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char          (isSpace,isAlpha,isDigit)
import Data.Attoparsec.ByteString.Char8 hiding (isDigit)
import           Bio.Phylogeny.PhyBin.CoreTypes (NewickTree(..), DefDecor, toLabel, treeSize)
import Control.Applicative

-- | Parse a bytestring into a NewickTree with branch lengths.  The
--   first argument is file from which the data came and is just for
--   error error messages.
parseNewick :: String -> B.ByteString -> NewickTree DefDecor
parseNewick file input = 
  runB file newick_parser $
  B.filter (not . isSpace) input

runB :: Show a => String -> Parser a -> B.ByteString -> a
runB file p input = case (parse p "" input) of
	         Left err -> error ("parse error in file "++ show file ++" at "++ show err)
		 Right x  -> x

----------------------------------------------------------------------------------------------------
-- Newick file format parser definitions:
----------------------------------------------------------------------------------------------------

tag :: a -> NewickTree a -> NewickTree a
tag l s =
  case s of 
    NTLeaf _ n      -> NTLeaf l n
    NTInterior _ ls -> NTInterior l ls

-- | This parser ASSUMES that whitespace has been prefiltered from the input.
newick_parser :: Parser (NewickTree DefDecor)
newick_parser = flip tag <$> subtree <*> branchMetadat <* char ';'

subtree :: Parser (NewickTree DefDecor)
subtree = internal <|> leaf

defaultMeta :: (Maybe Int, Double)
defaultMeta = (Nothing,0.0)

leaf :: Parser (NewickTree DefDecor)
leaf = NTLeaf <$> defaultMeta <*> name

internal :: Parser (NewickTree DefDecor)
internal = NTInterior <$> defaultMeta <*>
           char '(' *> brachset <* char ')' <* name

branchset :: Parser [NewickTree DefDecor]
branchset = (:) <$> (branch <?> "at least one branch") <*>
            option [] (char ',' *> branchset)

branch :: Parser (NewickTree DefDecor)
branch = tag <$> subtree <*> branchMetadat

-- If the length is omitted, it is implicitly zero.
branchMetadat :: Parser DefDecor
branchMetadat = option defaultMeta $ 
                (,) <$> char ':' *> double <*>
                option Nothing (Just <$> char '[' *> decimal <* char ']')

name :: Parser String
name = option "" $ many1 $
       satisfy (\c ->
                 isAlpha c || isDigit c ||
                 c == '.' || c == '_' ||
                 c == '-')

