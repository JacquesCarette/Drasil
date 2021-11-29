{-# LANGUAGE LambdaCase, Rank2Types #-}
-- | Defines functions to extract certain kinds of information from a document.
-- Mainly used to pull the 'UID's of chunks out of 'Sentence's and 'Expr's.
module Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, sentencePlate) where

import Control.Lens((^.))
import Drasil.DocumentLanguage.Core
import Drasil.Sections.SpecificSystemDescription (inDataConstTbl, outDataConstTbl)
import Language.Drasil hiding (Manual, Verb, constraints)
import Theory.Drasil
import Data.List(transpose)

import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (appendPlate, foldFor, purePlate, preorderFold)

-- | Creates a section contents plate that contains diferrent system subsections.
secConPlate :: Monoid b => (forall a. HasContents a => [a] -> b) ->
  ([Section] -> b) -> DLPlate (Constant b)
secConPlate mCon mSec = preorderFold $ purePlate {
  refSec = Constant <$> \(RefProg c _) -> mCon [c],
  iOrgSub = Constant <$> \(IOrgProg _ _ s _) -> mSec [s],
  --gsdSec = Constant <$> \case
  --  (GSDProg _) -> mempty,
  sysCntxtSub = Constant <$> \(SysCntxtProg c) -> mCon c,
  usrCharsSub = Constant <$> \(UsrCharsProg c) -> mCon c,
  systConsSub = Constant <$> \(SystConsProg c) -> mCon c,
  --CHECK later: problemDescription  = Constant <$> \(PDProg _ s _) -> mSec s,
  phySysDesc = Constant <$> \(PSDProg _ _ lc c) -> mCon [lc] `mappend` mCon c,
  constraints = Constant <$> \(ConstProg _ c) -> mCon [inDataConstTbl c],
  corrSolnPpties = Constant <$> \(CorrSolProg c cs) -> mCon [outDataConstTbl c] `mappend` mCon cs,
  reqSub = Constant <$> \case
    (FReqsSub' _ c) -> mCon c
    (FReqsSub _ c) -> mCon c
    (NonFReqsSub _) -> mempty,
  offShelfSec = Constant <$> \(OffShelfSolnsProg c) -> mCon c,
  appendSec = Constant <$> \(AppndxProg c) -> mCon c
}

-- | Creates a section plate for expressions.
exprPlate :: DLPlate (Constant [ModelExpr])
exprPlate = sentencePlate (concatMap sentToExp) `appendPlate` secConPlate (concatMap egetCon')
  (concatMap egetSec) `appendPlate` (preorderFold $ purePlate {
  tMs = Constant <$> \(TMProg _ _ t)   -> goTM t,
  dDs = Constant <$> \(DDProg _ _ d _) -> go d,
  gDs = Constant <$> \(GDProg _ _ g _) -> go g,
  iMs = Constant <$> \(IMProg _ _ i _) -> go i,
  auxConsSec = Constant <$> \(AuxConsProg _ qdef) -> go qdef
  }) where
      go :: Express a => [a] -> [ModelExpr]
      go = map express
      goTM :: [TheoryModel] -> [ModelExpr]
      goTM = concatMap (\x -> go (x ^. defined_quant)
                           ++ x ^. invariants
                           ++ go (map (^. defnExpr) (x ^. defined_quant ++ x ^. defined_fun))
                           ++ goTM (x ^. valid_context))

-- | Converts a 'Sentence' into a list of expressions. If the 'Sentence' cant be translated, returns an empty list.
sentToExp :: Sentence -> [ModelExpr]
sentToExp ((:+:) s1 s2) = sentToExp s1 ++ sentToExp s2
sentToExp (E e) = [e]
sentToExp _ = []

-- | Helper that extracts a list of some type from the 'DLPlate' and 'DocDesc'.
fmGetDocDesc :: DLPlate (Constant [a]) -> DocDesc -> [a]
fmGetDocDesc p = concatMap (foldFor docSec p)

-- | Extracts expressions from the document description ('DocDesc') and default 'DLPlate'.
egetDocDesc :: DocDesc -> [ModelExpr]
egetDocDesc = fmGetDocDesc exprPlate

-- | Extracts expressions from a 'Section'.
egetSec :: Section -> [ModelExpr]
egetSec (Section _ _ sc _ ) = concatMap egetSecCon sc

-- | Extracts expressions from section contents.
egetSecCon :: SecCons -> [ModelExpr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon' c

-- | Extracts expressions from something that has contents.
egetCon' :: HasContents a => a -> [ModelExpr]
egetCon' = egetCon . (^. accessContents)

-- | Extracts expressions from raw contents.
egetCon :: RawContent -> [ModelExpr]
egetCon (EqnBlock e) = [e]
egetCon (Defini _ []) = []
egetCon (Defini dt (hd:tl)) = concatMap egetCon' (snd hd) ++ egetCon (Defini dt tl)
egetCon _ = []

-- | Creates a 'Sentence' plate.
sentencePlate :: Monoid a => ([Sentence] -> a) -> DLPlate (Constant a)
sentencePlate f = appendPlate (secConPlate (f . concatMap getCon') $ f . concatMap getSec) $
  preorderFold $ purePlate {
    introSec = Constant . f <$> \(IntroProg s1 s2) -> [s1, s2],
    iPurposeSub = Constant . f <$> \(IPurposeProg s) -> s,
    iScopeSub = Constant . f <$> \(IScopeProg s) -> [s],
    iCharSub = Constant . f <$> \(ICharProg s1 s2 s3) -> concat [s1, s2, s3],
    iOrgSub = Constant . f <$> \(IOrgProg s1 _ _ s2) -> [s1, s2],
    clientSub = Constant . f <$> \(ClientProg _ s) -> [s],
    cstmrSub = Constant . f <$> \(CstmrProg _) -> [],
    problemDescription  = Constant . f <$> \(PDProg s) -> [s],
    termsAndDefs = Constant . f <$> \case
      (TDProg Nothing cs) -> def cs
      (TDProg (Just s) cs) -> s : def cs,
    phySysDesc = Constant . f <$> \(PSDProg _ s _ _) -> s,
    goals = Constant . f <$> \(GProg s c) -> s ++ def c,
    assumptions = Constant . f <$> \(AssumpProg c) -> def c,
    tMs = Constant . f <$> \(TMProg s _ t) -> let r = mappend s . concatMap (\x -> def (x ^. operations) ++
                                                   def (x ^. defined_quant) ++ notes [x] ++
                                                   r (x ^. valid_context)) in r t,
    dDs = Constant . f <$> \(DDProg s _ d _) -> s ++ der d ++ notes d,
    gDs = Constant . f <$> \(GDProg s _ d _) -> def d ++ s ++ der d ++ notes d,
    iMs = Constant . f <$> \(IMProg s _ d _) -> s ++ der d ++ notes d,
    constraints = Constant . f <$> \(ConstProg s _) -> [s],
    corrSolnPpties = Constant . f <$> \(CorrSolProg _ _) -> [],
    reqSub = Constant . f <$> \case
      (FReqsSub' c _) -> def c
      (FReqsSub c _) -> def c
      (NonFReqsSub c) -> def c,
    lcsSec = Constant . f <$> \(LCsProg c) -> def c,
    ucsSec = Constant . f <$> \(UCsProg c) -> def c,
    traceSec = Constant . f <$> \(TraceabilityProg progs) ->
      concatMap (\(TraceConfig _ ls s _ _) -> s : ls) progs,
    auxConsSec = Constant . f <$> \(AuxConsProg _ qdef) -> def qdef
  } where
    def :: Definition a => [a] -> [Sentence]
    def = map (^. defn)
    der :: HasDerivation a => [a] -> [Sentence]
    der = concatMap (getDerivSent . (^. derivations))
    getDerivSent :: Maybe Derivation -> [Sentence]
    getDerivSent Nothing = []
    getDerivSent (Just (Derivation h s)) = h : s
    notes :: HasAdditionalNotes a => [a] -> [Sentence]
    notes = concatMap (^. getNotes)

-- | Extracts 'Sentence's from a document description.
getDocDesc :: DocDesc -> [Sentence]
getDocDesc = fmGetDocDesc (sentencePlate id)

-- | Extracts 'Sentence's from a 'Section'.
getSec :: Section -> [Sentence]
getSec (Section _ t sc _ ) = t : concatMap getSecCon sc

-- | Extracts 'Sentence's from section contents.
getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon' c

-- | Extracts 'Sentence's from something that has contents.
getCon' :: HasContents a => a -> [Sentence]
getCon' = getCon . (^. accessContents)

-- | Extracts 'Sentence's from raw content.
getCon :: RawContent -> [Sentence]
getCon (Table s1 s2 t _) = isVar (s1, transpose s2) ++ [t]
getCon (Paragraph s)       = [s]
getCon EqnBlock{}          = []
getCon (DerivBlock h d)    = h : concatMap getCon d
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _)    = [l]
getCon (Bib bref)          = getBib bref
getCon (Graph [(s1, s2)] _ _ l) = [s1, s2, l]
getCon Graph{}             = []
getCon (Defini _ [])       = []
getCon (Defini dt (hd:fs)) = concatMap getCon' (snd hd) ++ getCon (Defini dt fs)

-- | This function is used in collecting 'Sentence's from a table.
-- Since only the table's first Column titled "Var" should be collected,
-- this function is used to filter out only the first column of 'Sentence's.
isVar :: ([Sentence], [[Sentence]]) -> [Sentence]
isVar (S "Var" : _, hd1 : _) = hd1
isVar (_ : tl, _ : tl1) = isVar (tl, tl1)
isVar ([], _) = []
isVar (_, []) = []

-- | Get the bibliography from something that has a field.
getBib :: (HasFields c) => [c] -> [Sentence]
getBib a = map getField $ concatMap (^. getFields) a

-- | Unwraps a 'CiteField' into a 'Sentence'.
getField :: CiteField -> Sentence
getField (Address s) = S s
getField Author{} = EmptyS
getField (BookTitle s) = S s
getField Chapter{} = EmptyS
getField Edition{} = EmptyS
getField Editor{} = EmptyS
getField HowPublished{} = EmptyS
getField (Institution s) = S s
getField (Journal s) = S s
getField Month{} = EmptyS
getField (Note s) = S s
getField Number{} = EmptyS
getField (Organization s) = S s
getField Pages{} = EmptyS
getField (Publisher s) = S s
getField (School s) = S s
getField (Series s) = S s
getField (Title s) = S s
getField (Type s) = S s
getField Volume{} = EmptyS
getField Year{} = EmptyS

-- | Translates different types of lists into a 'Sentence' form.
getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap (getIL . fst) it
getLT (Numeric it) = concatMap (getIL . fst) it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

-- | Translates a 'ListTuple' into 'Sentence's.
getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

-- | Flattens out an ItemType into 'Sentence's. Headers for 'Nested' items are prepended to its contents.
getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

-- ciPlate is not currently used. 
-- | A common idea plate.
-- ciPlate :: DLPlate (Constant [CI])
-- ciPlate = preorderFold $ purePlate {
--   introSub = Constant <$> \case
--     (IOrgSec _ ci _ _) -> [ci]
--     _ -> [],
--   stkSub = Constant <$> \case
--    (Client ci _) -> [ci]
--    (Cstmr ci) -> [ci],
--    auxConsSec = Constant <$> \(AuxConsProg ci _) -> [ci]
-- }
