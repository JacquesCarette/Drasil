{-# LANGUAGE LambdaCase, Rank2Types #-}
-- | Defines functions to extract certain kinds of information from a document.
-- Mainly used to pull the 'UID's of chunks out of 'Sentence's and 'Expr's.
module Drasil.ExtractDocDesc (
  getDocDesc, egetDocDesc,
  citeDBFromSections
) where

import Control.Lens((^.))
import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (appendPlate, foldFor, purePlate, preorderFold)
import qualified Data.Set as S

import Drasil.Database (UID)
import Language.Drasil hiding (getCitations, Manual, Verb)
import Language.Drasil.Development (lnames)
import Drasil.System (System, HasSystem(systemdb))
import Theory.Drasil

import Drasil.DocumentLanguage.Core hiding (System)
import Drasil.GetChunks (resolveBibliography)
import Drasil.Sections.SpecificSystemDescription (inDataConstTbl, outDataConstTbl)
import Drasil.ExtractCommon (sentToExp, getCon', getContList, egetCon)

-- | Creates a section contents plate that contains diferrent system subsections.
secConPlate :: Monoid b => (forall a. HasContents a => [a] -> b) ->
  ([Section] -> b) -> DLPlate (Constant b)
secConPlate mCon mSec = preorderFold $ purePlate {
  refSec = Constant <$> \(RefProg c _) -> mCon [c],
  introSub = Constant <$> \case
    (IOrgSec _ s _) -> mSec [s]
    _ -> mempty,
  gsdSub = Constant <$> \case
    (SysCntxt c) -> mCon c
    (UsrChars c) -> mCon c
    (SystCons c s) -> mCon c `mappend` mSec s,
  pdSec = Constant <$> \(PDProg _ s _) -> mSec s,
  pdSub = Constant <$> \case
    (TermsAndDefs _ _) -> mempty
    (PhySysDesc _ _ lc c) -> mCon [lc] `mappend` mCon c
    (Goals _ _) -> mempty,
  scsSub = Constant <$> \case
    (Constraints _ c) -> mCon [inDataConstTbl c]
    (CorrSolnPpties c cs) -> mCon [outDataConstTbl c] `mappend` mCon cs
    _ -> mempty,
  reqSub = Constant <$> \case
    (FReqsSub _ c) -> mCon c
    (NonFReqsSub _) -> mempty,
  offShelfSec = Constant <$> \(OffShelfSolnsProg c) -> mCon c,
  appendSec = Constant <$> \(AppndxProg c) -> mCon c
}

-- | Creates a section plate for expressions.
exprPlate :: DLPlate (Constant [ModelExpr])
exprPlate = sentencePlate (concatMap sentToExp) `appendPlate` secConPlate (concatMap egetCon)
  (concatMap egetSec) `appendPlate` (preorderFold $ purePlate {
  scsSub = Constant <$> \case
    (TMs _ _ t)   -> goTM t
    (DDs _ _ d _) -> go d
    (GDs _ _ g _) -> go g
    (IMs _ _ i _) -> go i
    _ -> [],
  auxConsSec = Constant <$> \(AuxConsProg _ qdef) -> go qdef
  }) where
      go :: Express a => [a] -> [ModelExpr]
      go = map express
      goTM :: [TheoryModel] -> [ModelExpr]
      goTM = concatMap (\x -> go (x ^. defined_quant)
                           ++ x ^. invariants
                           ++ go (map (^. defnExpr) (x ^. defined_quant ++ x ^. defined_fun))
                           ++ goTM (x ^. valid_context))

-- | Helper that extracts a list of some type from the 'DLPlate' and 'DocDesc'.
fmGetDocDesc :: DLPlate (Constant [a]) -> DocDesc -> [a]
fmGetDocDesc p = concatMap (foldFor docSec p)

-- | Extracts expressions from the document description ('DocDesc') and default 'DLPlate'.
egetDocDesc :: DocDesc -> [ModelExpr]
egetDocDesc = fmGetDocDesc exprPlate

-- | Extracts expressions from a 'Section'.
egetSec :: Section -> [ModelExpr]
egetSec (Section _ sc _ ) = concatMap egetSecCon sc

-- | Extracts expressions from section contents.
egetSecCon :: SecCons -> [ModelExpr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon c

-- | Creates a 'Sentence' plate.
sentencePlate :: Monoid a => ([Sentence] -> a) -> DLPlate (Constant a)
sentencePlate f = appendPlate (secConPlate (f . getContList) $ f . concatMap getSec) $
  preorderFold $ purePlate {
    introSec = Constant . f <$> \(IntroProg s1 s2 s3) -> [s1, s2] ++ concatMap getIntroSub s3,
    introSub = Constant . f <$> \case
      (IPurpose s) -> s
      (IScope s) -> [s]
      (IChar s1 s2 s3) -> concat [s1, s2, s3]
      (IOrgSec _ s1 s2) -> s2 : getSec s1,
    stkSub = Constant . f <$> \case
      (Client _ s) -> [s]
      (Cstmr _) -> [],
    pdSec = Constant . f <$> \(PDProg s secs pds) -> s : concatMap getSec secs ++ concatMap getPDSub pds,
    pdSub = Constant . f <$> \case
      (TermsAndDefs Nothing cs) -> def cs
      (TermsAndDefs (Just s) cs) -> s : def cs
      (PhySysDesc _ s lc cs) -> s ++ getCon' lc ++ getContList cs
      (Goals s c) -> s ++ def c,
    scsSub = Constant . f <$> \case
      (Assumptions c) -> def c
      (TMs s _ t) -> let r = (<>) s . concatMap (\x -> def (x ^. operations) ++
                             def (x ^. defined_quant) ++ notes [x] ++
                             r (x ^. valid_context)) in r t
      (DDs s _ d _) -> s ++ der d ++ notes d
      (GDs s _ d _) -> s ++ def d ++ der d ++ notes d
      (IMs s _ d _) -> s ++ der d ++ notes d
      (Constraints s _) -> [s]
      (CorrSolnPpties _ cs) -> getContList cs,
    reqSub = Constant . f <$> \case
      (FReqsSub c lcs) -> def c ++ getContList lcs
      (NonFReqsSub c) -> def c,
    lcsSec = Constant . f <$> \(LCsProg c) -> def c,
    ucsSec = Constant . f <$> \(UCsProg c) -> def c,
    traceSec = Constant . f <$> \(TraceabilityProg progs) ->
      concatMap (\(TraceConfig _ ls s _ _) -> s : ls) progs,
    auxConsSec = Constant . f <$> \(AuxConsProg _ qdef) -> def qdef
  } where
    def :: Definition a => [a] -> [Sentence]
    def = map (^. defn)

    getIntroSub :: IntroSub -> [Sentence]
    getIntroSub (IPurpose ss) = ss
    getIntroSub (IScope s) = [s]
    getIntroSub (IChar s1 s2 s3) = s1 ++ s2 ++ s3
    getIntroSub (IOrgSec _ s1 s2) = s2 : getSec s1

    der :: MayHaveDerivation a => [a] -> [Sentence]
    der = concatMap (getDerivSent . (^. derivations))

    getDerivSent :: Maybe Derivation -> [Sentence]
    getDerivSent Nothing = []
    getDerivSent (Just (Derivation h s)) = h : s

    notes :: HasAdditionalNotes a => [a] -> [Sentence]
    notes = concatMap (^. getNotes)

    getPDSub :: PDSub -> [Sentence]
    getPDSub (TermsAndDefs ms c) = def c ++ maybe [] pure ms
    getPDSub (PhySysDesc _ s lc cs) = s ++ getCon' lc ++ getContList cs
    getPDSub (Goals s c) = s ++ def c

-- | Extracts 'Sentence's from a document description.
getDocDesc :: DocDesc -> [Sentence]
getDocDesc = fmGetDocDesc (sentencePlate id)
-- ^ FIXME: We want all Sentences from a document (not necessarily a document
-- description), so we use this function. But 'sentencePlate' does not include
-- all 'Sentence's! Some only appear when rendering (at least, after
-- `mkSections` is used on a `DocDesc` to create `[Section]`).

-- | Extracts 'Sentence's from a 'Section'.
getSec :: Section -> [Sentence]
getSec (Section t sc _ ) = t : concatMap getSecCon sc

-- | Extracts 'Sentence's from section contents.
getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon' c

-- | Extracts citation reference 'UID's from generated sections. This is needed
-- because some sentences (like orgOfDocIntro) are only created when DocDesc is
-- converted to Sections during mkSections.
getCitationsFromSections :: [Section] -> S.Set UID
getCitationsFromSections ss = S.unions $ map (S.unions . map lnames . getSec) ss

-- | Extract bibliography entries from generated sections. This version extracts
-- from fully expanded Sections, capturing citations that are only created
-- during document generation (like those in orgOfDocIntro).
citeDBFromSections :: System -> [Section] -> BibRef
citeDBFromSections si = resolveBibliography (si ^. systemdb) . getCitationsFromSections
