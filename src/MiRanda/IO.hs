{-# LANGUAGE OverloadedStrings,BangPatterns #-}
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

import           Data.Attoparsec.ByteString.Char8
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
import           Control.Arrow
import           Text.Printf
import System.Exit
import Control.Concurrent.Async
import Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import MiRanda.Diagram
import MiRanda.Sheet.TargetSheet
import MiRanda.Sheet.SiteSheet
import Control.Monad
import System.Directory

toOutPut :: FilePath -> [Record] -> IO ()
toOutPut outPath rs =
    let dDir = "Diagrams"
        outP = outPath </> dDir
        targetFile = outPath </> "Target Genes.xml"
        siteFile = outPath </> "Target Sites.xml"
    in do
        mkdir outP
        a1 <- async $
              writeFile targetFile $ showSpreadsheet $
              mkTargetWorkbook ("." </> dDir) rs
        a2 <- async $
              writeFile siteFile $ showSpreadsheet $
              mkSiteWorkbook ("." </> dDir) rs
        a3 <- async $
              toDiagrams outP rs
              -- forM_ rs $ \r ->
              -- let mid = miRNA r
              --     sy = geneSymbol $ utr r
              --     re = refSeqID $ utr r
              --     base = B8.unpack
              --            (mid <> " vs " <>
              --             re <> "(" <> sy <> ")") <.> "pdf"
              --     outF = outP </> base
              -- in rend outF $ recordDiagram r
                 
        _ <- wait a1
        _ <- wait a2
        _ <- wait a3
        return ()

mkdir :: FilePath -> IO ()
mkdir fp = doesDirectoryExist fp >>=
           flip unless (createDirectory fp) 
    
toDiagrams :: FilePath -> [Record] -> IO ()
toDiagrams outP rs =
    forM_ rs $ \r ->
    let mid = miRNA r
        sy = geneSymbol $ utr r
        re = refSeqID $ utr r
        base = B8.unpack
               (mid <> " vs " <>
                re <> "(" <> sy <> ")") <.> "pdf"
        outF = outP </> base
    in rend outF $ recordDiagram r
    

-- TargetScan网站上的坐标是1based, NOT 0 based
-- | for debug use, most sites should be the same as targetscan 's out put
toTargetScanOutFormat :: [SiteLine] -> [B8.ByteString]
toTargetScanOutFormat =
    (headLine :) .
    map 
    (\sl ->
      let csp = context_Score_Plus sl
          toBS :: Show a => a -> B8.ByteString
          toBS = B8.pack . show
          toBS' = B8.pack . myShow
          toNum = B8.pack . printf "%.3f" 
          myShow e = case e of
              Nothing -> ""
              Just e -> printf "%.3f" e
          Con bool bl pct = conserve_Score sl
      in miRID sl <> "\t" <>
         syb (geneID sl) <> "\t" <> ref (geneID sl) <> "\t" <>
         
         (toBS $ (beg &&& end) $ seedRange sl) <> "\t" <>
         (toBS $ seed sl) <> "\t" <>
         (toBS' $ fmap siteTypeContribPlus csp) <> "\t" <>
         (toBS' $ fmap pairingContribPlus csp) <> "\t" <>
         (toBS' $ fmap localAUContribPlus csp) <> "\t" <>
         (toBS' $ fmap positionContribPlus csp) <> "\t" <>
         (toBS' $ fmap taContribPlus csp) <> "\t" <>
         (toBS' $ fmap spsContribPlus csp) <> "\t" <>
         (toBS' $ fmap contextPlus csp) <> "\t" <>
         (toNum bl) <> "\t" <>
         (B8.pack $ myShow pct) <> "\t" <> toBS bool
    ) . sortBy (compare `on` seedRange)
  where
    headLine = "#miRNA" <> "\t" <> "GeneSymbol" <> "\t" <>
               "RefSeq" <> "\t" <> "SiteRange" <> "\t" <>
               "SeedMatch" <> "\t" <> "SiteContrib" <> "\t" <>
               "3'Pairing" <> "\t" <> "LocalAU" <> "\t" <>
               "PosContrib" <> "\t" <> "TAContrib" <> "\t" <>
               "SPSContrib" <> "\t" <> "ContextPlus" <> "\t" <>
               "BL" <> "\t" <> "Pct" <> "\t" <> "IsConseved"

        


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
    (_,Just outH,_,pH) <- createProcess proP
    mRs <- return . mRecordFilter . toMRecords =<< B8.hGetContents outH
    ex <- waitForProcess pH
    case ex of
        ExitSuccess -> do
            utrSets <- readUTRs allUTRFile $! map _mRNA mRs
            return $ toRecord spe mRs utrSets
        _ -> exitWith ex

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
  snd $! fromJust $ find (\(a,b) -> s == B8.unpack a) $
  map (\(i,l) -> (commonName l,i)) $! IM.toList taxMap
{-# INLINE orgToTaxID #-}

toRecord :: String -> [MRecord] -> [[UTR]] -> [Record]
{-# INLINE toRecord #-}
toRecord str mRs utrSets =
  zipWith f mRs utrSets
  where
    toSites !mR !utr =
      let !mRL = mRNALen mR
      in map
         (\mSite ->
           let !mS = _mScore mSite
               !rawS = getRawScore utr mRL mSite
               !st = _seedType mSite
               !cS = getContextScore st rawS
               !csP = getContextScorePlus st al rawS
               !sR = getSeedMatchSite mSite
               !uR = _mRNARange mSite
               !m = _match mSite
               !al = _align mSite
           in Site mS rawS cS csP sR uR m st al
         ) (sites mR)
      
    f !mR !utrSet =
      let !utrID = _mRNA mR
          !miID = _miRNA mR
          (!lhs,!rhs) = partition ((== orgToTaxID str) . taxonomyID) utrSet
          !ss = toSites mR (head lhs)
      in if null lhs
         then error $ (show mR ++ "\n\n" ++ show utrSet)
         else Record miID utrID (head lhs) (rhs) ss
          
readUTRs :: FilePath -> [B8.ByteString] -> IO [[UTR]]
readUTRs !fp !genes =
  L8.readFile fp >>=
  return . myFilter genes .
  groupBy ((==) `on` refSeqID) . toUTRs
  where
    myFilter _ [] = []
    myFilter [] _ = []
    myFilter ag@(g:gs) (us:uss) =
      if refSeqID (head us) == g
      then us : myFilter gs uss
      else myFilter ag uss
           
toMRecords :: B8.ByteString -> [MRecord]
{-# INLINE toMRecords #-}
toMRecords = f . flip feed "" . parse parseRecords . preprocess
  where
    f (Done _ r) = r
    f e = error $ show e
    
preprocess :: B8.ByteString -> B8.ByteString
{-# INLINE preprocess #-}
preprocess = B8.intercalate "\n" . filter
             (\l ->
               B8.isPrefixOf "   " l ||
               B8.isPrefixOf ">" l
               ) .
             dropWhile
             (/= "Current Settings:") .
             B8.lines . B8.filter (/= '\r')

toUTRs :: ByteString -> [UTR]
{-# INLINE toUTRs #-}
toUTRs = map
         ((\(refId:_:syb:tax:sdata:[]) ->
            case L8.readInt tax of
              Just (taxId,_) -> UTR (L8.toStrict syb) (L8.toStrict refId) taxId (GS $ L8.toStrict sdata)
              _              -> error "Fail in parse UTR sequence."
          ) . (L8.split '\t')) . tail . L8.lines 

toFastas :: [UTR] -> ByteString
toFastas = toLazyByteString . intercalate '\n' .
           map (\utr ->
                 charUtf8 '>' <> byteString (refSeqID utr) <>
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
