{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2013 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------

module MiRanda.Sheet.Template
       (
         toGeneSheetComment
       , toSiteSheetComment
       ) 
       where

import Text.StringTemplate

toGeneSheetComment :: (Stringable a,ToSElem a) => a -> a -> a
{-# INLINE toGeneSheetComment #-}
toGeneSheetComment miRVersion geneRef =
    render $
    setManyAttrib
    [("miRBaseVersion",miRVersion)
    ,("geneRefSeqID",geneRef)]
    geneSheetTemplate

toSiteSheetComment :: (Stringable a,ToSElem a) => a -> a -> a
{-# INLINE toSiteSheetComment #-}
toSiteSheetComment miRVersion geneRef =
    render $
    setManyAttrib
    [("miRBaseVersion",miRVersion)
    ,("geneRefSeqID",geneRef)]
    siteSheetTemplate

geneSheetTemplate :: Stringable a => StringTemplate a
geneSheetTemplate =
    newSTMP
    "Predicted miRNAs bind to $geneRefSeqID$\n\
    \\n\n\
    \# Column A: Identity, the name of the mature miRNA in miRBase v$miRBaseVersion$.\n\
    \# Column B: Accession, the mature accession id used in miRBase v$miRBaseVersion$.\n\
    \# Column C: Diagrams, the diagrams illustrate key features for miRNA binding. See Ref [1,2] for details.\n\
    \# Column D: Total, the total number of the binding sites on $geneRefSeqID$.\n\
    \# Column E: Context+, the sum of the context+ scores used in TargetScan after version 6.0. More negative is better. See Ref [3,4] for details.\n\
    \# Column F: Context, the sum of the context scores used in TargetScan before version 5.x. More negative is better. See Ref [1] for details.\n\
    \# Column G: Structure, the sum of the structure scores used in miRanda (Ref [5]). The higher the better.\n\
    \# Column H: Energy, the sum of the free energy predicted by miRanda (Ref [5]). More negative is better.\n\
    \# Column I: Branch Length, the sum of the branch length in the phylogenetic tree of different species. The higher \
    \means the binding site is more conserved (Ref [3]).\n\
    \# Column J: Pct, the probability of preferentially conserved targeting (Ref [3]).  This score reflected the \
    \Bayesian estimate of the probability that a site is conserved due to selective maintenance of miRNA \
    \targeting rather than by chance or any other reason not pertinent to miRNA targeting.\n\
    \# Column K ~ Column V: Counting numbers for different seed match types.\n\
    \# Column W: Rfam, the Rfam family annotations.\n\
    \# Column X: miRBase, the miRBase family annotations.\n\
    \# Column Y: Family, the family name for the miRNA.\n\
    \# Column Z: isExperimentalValidated, whether this miRNA is validated by experimental evidence.\n\
    \# Column AA: Sequence, the sequence for the miRNA.\n\
    \\n\n\
    \# Note:\n\
    \Currently, there is not enough data for constructing phylogenetic tree of different species for LncRNAs. The \"Conservation\" \
    \section is meaningless for LncRNA. All sites on LncRNA are treated as non-conserved ones.\n\
    \\n\n\
    \# Reference:\n\
    \[1] Grimson A., et al. MicroRNA Targeting Specificity in Mammals: Determinants beyond Seed Pairing. \
    \Molecular Cell, Volume 27, Issue 1, 91-105, 6 July 2007.\n\
    \[2] Amy E. Pasquinelli. MicroRNAs and their targets: recognition, regulation and an emerging reciprocal relationship. \
    \Nature Reviews Genetics, Volume 13, 271-282, 1 April 2012.\n\
    \[3] Robin C Friedman, et al. Most Mammalian mRNAs Are Conserved Targets of MicroRNAs. \
    \Genome Research, Volume 19, 92-105, 2009.\n\
    \[4] David M García., et al. Weak Seed-Pairing Stability and High Target-Site Abundance Decrease the Proficiency of lsy-6 and Other miRNAs. \
    \Nat. Struct. Mol. Biol., Volume 18, 1139-1146, 2011.\n\
    \[5] Enright AJ., el al. miRanda algorithm: MicroRNA targets in Drosophila. \
    \Genome Biology, Volume 5, R1, 2003.\n\n"

siteSheetTemplate :: Stringable a => StringTemplate a
siteSheetTemplate =
    newSTMP
    "Predicted miRNA Sites on $geneRefSeqID$.\n\n\
    \This sheet is intended for advanced users who want \
    \more fine-grained control over filtering sites or building new site prodiction models.\n\
    \\n\n\
    \# Column A: Identity, the name of the mature miRNA in miRBase v$miRBaseVersion$.\n\
    \# Column B: Accession, the mature accession id used in miRBase v$miRBaseVersion$.\n\
    \# Column C: Diagrams, the diagrams illustrate key features for miRNA binding. See Ref [1,2] for details.\n\
    \# Column D: Range, the position of the site. For coding genes, this is the position on 3'UTR. For LncRNAs, \
    \this is the position on the full transcripts.\n\
    \# Column E: Seed Match, the type for the seed match.\n\
    \# Column F ~ K: The detialed components of the context+ score. See Ref [4] for details.\n\
    \# Column L: The context+ score for the site.\n\
    \# Column M ~ P: The detialed components of the context score. See Ref [1] for details.\n\
    \# Column Q: The context score for the site.\n\
    \# Column R ~ T: The detialed components for the 3'-Pairing, Local AU and Position scores defined in Ref [1].\n\
    \# Column U,V: The thermodynamic properties of the binding site given by miRanda (Ref [5]).\n\
    \# Column W ~ Y: The conservation score used in TargetScan. See Ref [3] for details.\n\
    \\n\n\
    \# Note:\n\
    \Currently, there is not enough data for constructing phylogenetic tree of different species for LncRNAs. The \"Conservation\" \
    \section is meaningless for LncRNA. All sites on LncRNA are treated as non-conserved ones.\n\
    \\n\n\
    \# Reference:\n\
    \[1] Grimson A., et al. MicroRNA Targeting Specificity in Mammals: Determinants beyond Seed Pairing. \
    \Molecular Cell, Volume 27, Issue 1, 91-105, 6 July 2007.\n\
    \[2] Amy E. Pasquinelli. MicroRNAs and their targets: recognition, regulation and an emerging reciprocal relationship. \
    \Nature Reviews Genetics, Volume 13, 271-282, 1 April 2012.\n\
    \[3] Robin C Friedman, et al. Most Mammalian mRNAs Are Conserved Targets of MicroRNAs. \
    \Genome Research, Volume 19, 92-105, 2009.\n\
    \[4] David M García., et al. Weak Seed-Pairing Stability and High Target-Site Abundance Decrease the Proficiency of lsy-6 and Other miRNAs. \
    \Nat. Struct. Mol. Biol., Volume 18, 1139-1146, 2011.\n\
    \[5] Enright AJ., el al. miRanda algorithm: MicroRNA targets in Drosophila. \
    \Genome Biology, Volume 5, R1, 2003.\n\n"
