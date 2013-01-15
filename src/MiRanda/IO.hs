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

module MiRanda.IO where

import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char
import           Data.Function
import qualified Data.IntMap.Strict as IM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           MiRanda.Parameter
import           MiRanda.Parser
import           MiRanda.Types
import MiRanda.Util
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           MiRanda.Score


mkProcess :: FilePath -> FilePath -> IO CreateProcess
mkProcess miRFile utrFile = do
  prog <- findExecutable "miranda"
  case prog of
    Nothing -> error "Cant find miranda in PATH."
    Just program ->
      let cmdSpec = RawCommand program [miRFile,utrFile]
      in return $
         CreateProcess cmdSpec Nothing Nothing
         Inherit (CreatePipe) Inherit False False

      

miRNAPredict :: String -> Fasta -> FilePath -> IO [Record]
miRNAPredict spe miFasta@(Fasta sid _) allUTRFile =
  do
    tmpDir <- getTemporaryDirectory
    let miFile = tmpDir </> B8.unpack sid <.> "fa"
        utrFile = tmpDir </> B8.unpack sid <.> "utr"
    writeMiR miFile miFasta
    dumpGenome spe allUTRFile utrFile
    proP <- mkProcess miFile utrFile
    (_,Just outH,_,_) <- createProcess proP
    mRs <- return . toMRecords =<< L8.hGetContents outH
    utrSets <- readUTRs allUTRFile (map _mRNA mRs)
    return $ toRecord spe mRs utrSets

dumpGenome :: String -> FilePath -> FilePath -> IO ()
dumpGenome specName inFile outFile =
  L8.readFile inFile >>=
  L8.writeFile outFile . toFastas .
  filter
  (\utr ->
    taxonomyID utr == orgToTaxID specName
  ) . toUTRs . L8.filter (/= '\r')
  
orgToTaxID :: String -> Int
orgToTaxID s =
  snd $ fromJust $ find (\(a,b) -> s == B8.unpack a) $
  map (\(i,l) -> (commonName l,i)) $ IM.toList taxMap

toRecord :: String -> [MRecord] -> [[UTR]] -> [Record]
toRecord str mRs utrSets =
  zipWith f mRs utrSets
  where
    toSites mR utr =
      let mRL = mRNALen mR
      in map
         (\mSite ->
           let mS = _mScore mSite
               rawS = getRawScore utr mRL mSite
               st = _seedType mSite
               cS = getContextScore st rawS
               csP = getContextScorePlus st al rawS
               sR = getSeedMatchSite mSite
               uR = _mRNARange mSite
               m = _match mSite
               al = _align mSite
           in Site mS rawS cS csP sR uR m st al
         ) (sites mR)
      
    f mR utrSet =
      let utrID = _mRNA mR
          miID = _miRNA mR
          (lhs,rhs) = partition ((== orgToTaxID str) . taxonomyID) utrSet
          ss = toSites mR (head lhs)
      in if null lhs
         then error $ (show mR ++ "\n\n" ++ show utrSet)
         else Record miID utrID (head lhs) (rhs) ss
          

readUTRs :: FilePath -> [B8.ByteString] -> IO [[UTR]]
readUTRs fp genes =
  L8.readFile fp >>=
  return . myFilter genes .
  groupBy ((==) `on` geneSymbol) . toUTRs
  where
    myFilter _ [] = []
    myFilter [] _ = []
    myFilter ag@(g:gs) (us:uss) =
      if geneSymbol (head us) == g
      then us : myFilter gs uss
      else myFilter ag uss
           
toMRecords :: ByteString -> [MRecord]
toMRecords = f . parse parseRecords . preprocess
  where
    f (Done _ r) = r
    f e = error $ show e
    
preprocess :: ByteString -> ByteString
preprocess = L8.intercalate "\n" . filter
             (\l ->
               L8.isPrefixOf "   " l ||
               L8.isPrefixOf ">" l
               ) .
             dropWhile
             (/= "Current Settings:") .
             L8.lines . L8.filter (/= '\r')

toUTRs :: ByteString -> [UTR]
toUTRs = map
         ((\(_:_:syb:tax:sdata:[]) ->
            case L8.readInt tax of
              Just (taxId,_) -> UTR (L8.toStrict syb) taxId (GS $ L8.toStrict sdata)
              _              -> error "Fail in parse UTR sequence."
          ) . (L8.split '\t')) . tail . L8.lines 

toFastas :: [UTR] -> ByteString
toFastas = toLazyByteString . intercalate '\n' .
           map (\utr ->
                 charUtf8 '>' <> byteString (geneSymbol utr) <>
                 charUtf8 '\n' <>
                 alignToSeq utr <>
                 charUtf8 '\n'
               )
  where
    lineLen = 70
    intercalate _ [] = mempty
    intercalate c (b:bs) = b <> charUtf8 c <>
                           intercalate c bs
    alignToSeq = splitEvery 70 . B8.filter isAlpha .
                 (\(GS str) -> str) . alignment
    splitEvery n bstr = let (h,t) = B8.splitAt n bstr
                        in if B8.null t
                           then byteString h
                           else byteString h <>
                                charUtf8 '\n' <>
                                splitEvery n t

countGene :: ByteString -> Int
countGene str = if L8.null str
                then 0
                else go 0 $ L8.findIndices (== '>') str
  where
    go acc [] = acc
    go acc [i] = acc
    go acc (i:j:is) =
      if j - i == 1
      then (acc+1) `seq` go (acc+1) (j:is)
      else go acc (j:is)

readFasta :: FilePath -> IO [Fasta]
readFasta fp =
  L8.readFile fp >>=
  return .
  map
  (\(h:sd) ->
    Fasta (L8.toStrict $ L8.tail h) (SD $ B8.concat $ map (L8.toStrict . L8.filter isAlpha) sd)) .
  groupBy (\_ b -> L8.head b /= '>') .
  filter (not . L8.null) . L8.lines . L8.filter (/= '\r')

writeMiR :: FilePath -> Fasta -> IO ()
writeMiR fp f =
  B8.writeFile fp $ 
  (B8.cons '>' $ seqlabel f) `B8.append` "\n" `B8.append`
  (unSD $ seqdata f)
