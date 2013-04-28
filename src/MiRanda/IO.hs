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

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Parallel.Strategies
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
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           GHC.Conc
import           MiRanda.Diagram
import           MiRanda.Parameter
import           MiRanda.Parser
import           MiRanda.Score
import           MiRanda.Sheet.SiteSheet
import           MiRanda.Sheet.TargetSheet
import           MiRanda.Types
import           MiRanda.Util
import           System.Directory
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf
import           Text.XML.SpreadsheetML.Writer (showSpreadsheet)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
data Token = Token

toOutPut :: String -> FilePath -> FilePath -> (FilePath -> [Record] -> IO ()) -> [MRecord] -> IO ()
{-# INLINE toOutPut #-}
toOutPut spe allUTRFile outPath func mRs =
    let dDir = "Diagrams"
        outP = outPath </> dDir
        targetFile = outPath </> "Target Genes.xml"
        siteFile = outPath </> "Target Sites.xml"
        hRef = dDir
        nThreadDraw = 1
        -- cairo 1.12.10 并发写pdf导致吐核
        -- 也许以后可以,这是一个主要性能瓶颈
        -- testIO: cairo-hash.c:506:
        -- _cairo_hash_table_lookup_exact_key: Assertion `!"reached"' failed.
        n = nThreadDraw + 2
    in do
        (rfs,sis) <- readUTRs allUTRFile (map _mRNA mRs) >>=
              return .
              (toRefLines &&& toSiteLines) .
              (\ss -> zip ss (getConservations ss)) . recordFilter . toRecord spe mRs
        mkdir outP
        
        allMVs@(mv1:mv2:mvs) <- sequence $ replicate n newEmptyMVar

        _ <- forkOS $ do
            writeFile targetFile $ showSpreadsheet $
                mkTargetWorkbook hRef rfs
            putMVar mv1 Token
            
        _ <- forkOS $ do
            writeFile siteFile $ showSpreadsheet $
                mkSiteWorkbook hRef sis
            putMVar mv2 Token

        rss <- readUTRs allUTRFile (map _mRNA mRs) >>= 
               return . zip mvs .
               splitList nThreadDraw . recordFilter . toRecord spe mRs
               
        forM_ rss $ \(mv,rs) -> 
            forkOS $ do
                func outP rs
                putMVar mv Token

        sequence_ $ map takeMVar allMVs

splitList :: Int -> [a] -> [[a]]
splitList n ls =
    let ls' = zip [0..] ls
        ns = [0..n-1]
    in map
       (\i ->
         map snd $ filter ((== i) . (`mod` n) . fst) ls'
       ) ns

recordFilter :: [Record] -> [Record]
{-# INLINE recordFilter #-}
recordFilter rs =
    map fst $
    filter ( not . null . snd) $
    map
    (\(r,cons) ->
      let ss = predictedSites r
          (ss',cons') = unzip $
                        filter
                        (\(s,con) ->
                          let scoreBool = 
                                  case contextScorePlus s of
                                        Nothing -> True
                                            -- 6mer,6mer offset and imperfect seed match
                                            -- if branchLength con >= stringentBranchLengthCutOff
                                            -- then True
                                            -- else False
                                        Just csp ->
                                            if contextPlus csp < 0 
                                            then True
                                            else False
                              (P b e) = seedMatchRange s
                              posBool = b >= 30 &&
                                        (l - e) >= 30
                          in scoreBool && posBool
                        ) $ zip ss cons
          l = length $ B8.findIndices isAlpha $ extractSeq $ utr r
          r' = r {predictedSites = ss'}
      in (r',cons')
    ) $ zip rs $ getConservations rs
  where 
    stringentBranchLengthCutOff :: Double
    stringentBranchLengthCutOff = 0.5
    branchLengthCutOff :: Double
    branchLengthCutOff = 0.3


mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

toDiagramsLnc :: FilePath -> [Record] -> IO ()
toDiagramsLnc outP rs =
    mapM_ (\(outF,d) -> renderPDF outF d) $
    withStrategy (parBuffer numCapabilities rseq) $
    map 
    (\r ->
      let mid = miRNA r
          sy = geneSymbol $ utr r
          re = refSeqID $ utr r
          base = B8.unpack
                 (mid <> " vs " <>
                  re <> "(" <> sy <> ")") <.> "pdf"
          outF = outP </> base
          d = tableDiagram r
      in (outF,d)) rs

toDiagrams :: FilePath -> [Record] -> IO ()
toDiagrams outP rs =
    mapM_ (\(outF,d) -> renderPDF outF d) $
    withStrategy (parBuffer numCapabilities rseq) $
    map 
    (\r ->
      let mid = miRNA r
          sy = geneSymbol $ utr r
          re = refSeqID $ utr r
          base = B8.unpack
                 (mid <> " vs " <>
                  re <> "(" <> sy <> ")") <.> "pdf"
          outF = outP </> base
          d = recordDiagram r
      in (outF,d)) rs
    

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

miRNAPredict :: String -> Fasta -> FilePath -> IO [MRecord]
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
        ExitSuccess -> return mRs
        _ -> exitWith ex

mRecordFilter :: [MRecord] -> [MRecord]
{-# INLINE mRecordFilter #-}
mRecordFilter mrs =
    filter
    (\mr ->
      if null (sites mr)
      then False
      else True) $
    map
    (\mr ->
      let ss = sites mr
          ss' = filter
                (\s ->
                  case _seedType s of
                      Imperfect ->
                          let (PairScore p) = getPairScore Imperfect $!
                                              _align s
                          in if p >= 2
                             then True
                             else False
                      other -> True
                ) ss
      in mr {sites=ss'}) mrs

dumpUTRs :: String -> FilePath -> [UTR] -> IO ()
dumpUTRs specName  outFile =
    L8.writeFile outFile . toFastas .
    filter
    (\utr ->
      taxonomyID utr == orgToTaxID specName
    )
    
dumpGenome :: String -> FilePath -> FilePath -> IO ()
dumpGenome specName inFile outFile =
  L8.readFile inFile >>=
  dumpUTRs specName outFile . toUTRs . L8.filter (/= '\r')
  
orgToTaxID :: String -> Int
orgToTaxID !s =
  snd $! fromJust $ find (\(a,b) -> s == B8.unpack a) $
  map (\(i,l) -> (commonName l,i)) $! IM.toList taxMap
{-# INLINE orgToTaxID #-}


toRecord :: String -> [MRecord] -> [[UTR]] -> [Record]
{-# INLINE toRecord #-}
toRecord str mRs utrSets =
  zipWith f mRs utrSets
  where
    f mR utrSet =
      let utrID = _mRNA mR
          miID = _miRNA mR
          (lhs,rhs) = partition ((== orgToTaxID str) . taxonomyID) utrSet
          ss = toSites (mRNALen mR) (head lhs) (sites mR)
      in if null lhs
         then error $ (show mR ++ "\n\n" ++ show utrSet)
         else Record miID utrID (head lhs) (rhs) ss

toSites :: Int -> UTR -> [MSite] -> [Site]
toSites mRNALength utr =
    map
    (\mSite ->
      let mS = _mScore mSite
          rawS = getRawScore utr mRNALength mSite
          st = _seedType mSite
          cS = getContextScore st rawS
          csP = getContextScorePlus st al rawS
          sR = getSeedMatchSite mSite
          uR = _mRNARange mSite
          m = _match mSite
          al = _align mSite
      in Site mS rawS cS csP sR uR m st al)


readUTRs :: FilePath -> [B8.ByteString] -> IO [[UTR]]
readUTRs fp genes =
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
         (f . (L8.split '\t')) . tail . L8.lines . L8.filter (/= '\r')
  where f (refId:_:syb:tax:sdata:[]) = case L8.readInt tax of
                                       Just (taxId,_) -> UTR (L8.toStrict syb) (L8.toStrict refId) taxId (GS $ L8.toStrict sdata)
                                       _              -> error "Fail in parse UTR sequence."
        f other = error $ show other


toFastas :: [UTR] -> ByteString
toFastas = renderFastas .
           filter ((/= 0) . B8.length . unSD . seqdata) .
           map (\utr ->
                 Fasta (refSeqID utr) $ SD $ B8.filter isAlpha $
                 unGS $ alignment utr
               )

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

-- quick hack
mkUTRFile :: String -> FilePath -> FilePath -> FilePath -> IO ()
mkUTRFile spe arrayFasta arrayAnno outUTR = do
    fseqs <- readFasta arrayFasta
    str <- L8.readFile arrayAnno
    let fHash = H.fromList $ zip (map (L8.fromStrict.seqlabel) fseqs) $ map (L8.fromStrict . unSD . seqdata) fseqs
        atV = (V.!)
        h = "Refseq ID\tGene ID\tGene Symbol\tSpecies ID\tUTR sequence"
        ls = map
             (\v ->
                L8.intercalate "\t"
                [v `atV` 2 -- Seqname
                ,v `atV` 0 -- ProbeID
                ,v `atV` 3 -- GeneSymbol
                ,L8.pack $ show $ orgToTaxID spe
                ,fHash H.! (v `atV` 2)] 
             ) $
             sortBy (compare `on` (`atV` 0)) $
             map V.fromList $
             filter
             (\ls ->
               length ls > 1 &&
               (head $ tail ls) == "noncoding"
             ) $
             map (L8.split '\t') $
             tail $ L8.lines $ L8.filter (/= '\r') str
    L8.writeFile outUTR $ L8.unlines $ h:ls
