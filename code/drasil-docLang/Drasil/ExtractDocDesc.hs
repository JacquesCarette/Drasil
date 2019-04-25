module Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, ciGetDocDesc) where

import Control.Lens((^.))
import Drasil.DocumentLanguage
import Language.Drasil hiding (Manual, Vector, Verb)
import Data.List(transpose)

egetDocDesc :: DocDesc -> [Expr]
egetDocDesc d = concatMap egetDocSec d

egetDocSec :: DocSection -> [Expr]
egetDocSec (Verbatim a)         = egetSec a
egetDocSec (RefSec r)           = egetRefSec r
egetDocSec IntroSec{}           = []
egetDocSec (StkhldrSec s)       = egetStk s
egetDocSec (GSDSec g)           = egetGSD g
egetDocSec (ScpOfProjSec s)     = egetScp s
egetDocSec (SSDSec s)           = egetSSD s
egetDocSec (ReqrmntSec r)       = egetReq r
egetDocSec (LCsSec l)           = egetLcs l
egetDocSec LCsSec'{}            = [] -- likely changes can't lead to Expr?
egetDocSec (UCsSec u)           = egetUcs u
egetDocSec (TraceabilitySec t)  = egetTrace t
egetDocSec (AuxConstntSec a)    = egetAux a
egetDocSec (Bibliography)       = []
egetDocSec (AppndxSec a)        = egetApp a
egetDocSec (ExistingSolnSec e)  = egetExist e

egetSec :: Section -> [Expr]
egetSec (Section _ sc _ ) = concatMap egetSecCon sc

egetSecCon :: SecCons -> [Expr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon' c

egetCon' :: Contents -> [Expr]
egetCon' c = egetCon (c ^. accessContents)

egetCon :: RawContent -> [Expr]
egetCon (EqnBlock e) = [e]
egetCon (Defini _ []) = []
egetCon (Defini dt (hd:tl)) = concatMap egetCon' (snd hd) ++ egetCon (Defini dt tl)
egetCon _ = []

egetLblCon :: LabelledContent -> [Expr]
egetLblCon a = egetCon (a ^. accessContents)

egetRefSec :: RefSec -> [Expr]
egetRefSec (RefProg c r) = egetCon' c ++ concatMap egetRefProg r

egetGSD :: GSDSec -> [Expr]
egetGSD (GSDProg s1 c1 c2 s2) = concatMap egetSec s1 ++ egetCon' c1
  ++ concatMap egetCon' c2 ++ concatMap egetSec s2
egetGSD (GSDProg2 gsdsub) = concatMap egetGSDSub gsdsub

egetScp :: ScpOfProjSec -> [Expr]
egetScp (ScpOfProjProg _ c1 c2) = egetCon' c1 ++ egetCon' c2

egetSSD :: SSDSec -> [Expr]
egetSSD (SSDProg ssd) = concatMap egetSSDSub ssd

egetReq :: ReqrmntSec -> [Expr]
egetReq (ReqsProg rs) = concatMap egetReqSub rs

egetLcs :: LCsSec -> [Expr]
egetLcs (LCsProg c) = concatMap egetCon' c

egetUcs :: UCsSec -> [Expr]
egetUcs (UCsProg c) = concatMap egetCon' c

egetTrace :: TraceabilitySec -> [Expr]
egetTrace (TraceabilityProg lc _ c s) = concatMap egetLblCon lc ++ concatMap egetCon' c
  ++ concatMap egetSec s

egetAux :: AuxConstntSec -> [Expr]
egetAux (AuxConsProg _ qd) = concatMap egetQDef qd

egetQDef :: QDefinition -> [Expr]
egetQDef q = [q ^. defnExpr]

egetApp :: AppndxSec -> [Expr]
egetApp (AppndxProg c) = concatMap egetCon' c

egetExist :: ExistingSolnSec -> [Expr]
egetExist (ExistSolnVerb s) = egetSec s
egetExist (ExistSolnProg c) = concatMap egetCon' c

egetRefProg :: RefTab -> [Expr]
egetRefProg TUnits       = []
egetRefProg TUnits'{}    = []
egetRefProg TSymb{}      = []
egetRefProg (TSymb' l _) = egetFunc l
egetRefProg TAandA       = []

egetStk :: StkhldrSec -> [Expr]
egetStk StkhldrProg{}     = []
egetStk (StkhldrProg2 s)  = concatMap egetStkSub s

egetStkSub :: StkhldrSub -> [Expr]
egetStkSub _ = []

egetGSDSub :: GSDSub -> [Expr]
egetGSDSub (SysCntxt c)   = concatMap egetCon' c
egetGSDSub (UsrChars c)   = concatMap egetCon' c
egetGSDSub (SystCons c s) = concatMap egetCon' c ++ concatMap egetSec s

egetSSDSub :: SSDSub -> [Expr]
egetSSDSub (SSDSubVerb s)   = egetSec s
egetSSDSub (SSDProblem p)   = egetProblem p
egetSSDSub (SSDSolChSpec s) = egetSol s

egetReqSub :: ReqsSub -> [Expr]
egetReqSub (FReqsSub c) = concatMap egetCon' c
egetReqSub NonFReqsSub{} = []

egetFunc :: LFunc -> [Expr]
egetFunc Term         = []
egetFunc Defn         = []
egetFunc TermExcept{} = []
egetFunc DefnExcept{} = []
egetFunc TAD          = []

egetProblem :: ProblemDescription -> [Expr]
egetProblem (PDProg _ _ _ s) = concatMap egetSec s

egetSol :: SolChSpec -> [Expr]
egetSol (SCSProg s) = concatMap egetSCSSub s

egetSCSSub :: SCSSub -> [Expr]
egetSCSSub Assumptions  = []
egetSCSSub (TMs _ x)    = concatMap egetTM x
egetSCSSub (GDs _ x _)  = concatMap egetGD x
egetSCSSub (DDs _ _ x _)  = concatMap egetDD x
egetSCSSub (IMs _ _ x _)  = concatMap egetIM x
egetSCSSub (Constraints _ _ _ lc) = concatMap egetLblCon lc
egetSCSSub (CorrSolnPpties c) = concatMap egetCon' c

egetTM :: TheoryModel -> [Expr]
egetTM x = concatMap egetTM (x ^. valid_context) ++ 
  concatMap egetQDef (x ^. defined_quant ++ x ^. defined_fun)
  ++ (x ^. invariants)

egetIM :: InstanceModel ->[Expr]
egetIM x = [x ^. relat]

egetGD :: GenDefn ->[Expr]
egetGD gd = [gd ^. relat]

egetDD :: DataDefinition -> [Expr]
egetDD dd = [dd ^. defnExpr, sy dd]

getDocDesc :: DocDesc -> [Sentence]
getDocDesc d = concatMap getDocSec d

getDocSec :: DocSection -> [Sentence]
getDocSec (Verbatim a)         = getSec a
getDocSec (RefSec r)           = getRefSec r
getDocSec (IntroSec i)         = getIntrosec i
getDocSec (StkhldrSec s)       = getStk s
getDocSec (GSDSec g)           = getGSD g
getDocSec (ScpOfProjSec s)     = getScp s
getDocSec (SSDSec s)           = getSSD s
getDocSec (ReqrmntSec r)       = getReq r
getDocSec (LCsSec l)           = getLcs l
getDocSec (LCsSec' l)          = getLcs' l
getDocSec (UCsSec u)           = getUcs u
getDocSec (TraceabilitySec t)  = getTrace t
getDocSec (AuxConstntSec a)    = getAux a
getDocSec Bibliography         = []
getDocSec (AppndxSec a)        = getApp a
getDocSec (ExistingSolnSec e)  = getExist e

getSec :: Section -> [Sentence]
getSec (Section t sc _ ) = t : concatMap getSecCon sc

getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon' c

getCon' :: Contents -> [Sentence]
getCon' c = getCon (c ^. accessContents)

getCon :: RawContent -> [Sentence]
getCon (Table s1 s2 t _) = isVar (s1, transpose s2) ++ [t]
getCon (Paragraph s)       = [s]
getCon EqnBlock{}          = []
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _)    = [l]
getCon (Bib bref)          = getBib bref
getCon (Graph [(s1, s2)] _ _ l) = s1 : s2 : [l]
getCon Graph{}             = []
getCon (Defini _ [])       = []
getCon (Defini dt (hd:fs)) = concatMap getCon' (snd hd) ++ getCon (Defini dt fs)

-- This function is used in collecting sentence from table.
-- Since only the table's first Column titled "Var" should be collected,
-- this function is used to filter out only the first Column of Sentence.
isVar :: ([Sentence], [[Sentence]]) -> [Sentence]
isVar (S "Var" : _, hd1 : _) = hd1
isVar (_ : tl, _ : tl1) = isVar (tl, tl1)
isVar ([], _) = []
isVar (_, []) = []

getBib :: (HasFields c) => [c] -> [Sentence]
getBib a = map getField $ concatMap (^. getFields) a

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

getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap (getIL . fst) it
getLT (Numeric it) = concatMap (getIL . fst) it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

getRefSec :: RefSec -> [Sentence]
getRefSec (RefProg c r) = getCon' c ++ concatMap getReftab r

getReftab :: RefTab -> [Sentence]
getReftab TUnits = []
getReftab (TUnits' tu) = concatMap getTuIntro tu
getReftab (TSymb ts) = concatMap getTsIntro ts
getReftab (TSymb' lf ts) = getLFunc lf ++ concatMap getTsIntro ts
getReftab TAandA = []

getTuIntro :: TUIntro -> [Sentence]
getTuIntro System    = []
getTuIntro Derived   = []
getTuIntro TUPurpose = []

getTsIntro :: TSIntro -> [Sentence]
getTsIntro (TypogConvention tc) = concatMap getTConv tc
getTsIntro SymbOrder            = []
getTsIntro SymbConvention{}     = []
getTsIntro TSPurpose            = []

getLFunc :: LFunc -> [Sentence]
getLFunc Term = []
getLFunc Defn = []
getLFunc (TermExcept x) = map (^. defn) x
getLFunc (DefnExcept x) = map (^. defn) x
getLFunc TAD  = []

getTConv :: TConvention -> [Sentence]
getTConv Vector{} = []
getTConv (Verb s) = [s]

getIntrosec :: IntroSec -> [Sentence]
getIntrosec (IntroProg s1 s2 is) = [s1] ++ [s2] ++ concatMap getIntroSub is

getIntroSub :: IntroSub -> [Sentence]
getIntroSub (IPurpose s) = [s]
getIntroSub (IScope s1 s2) = [s1, s2]
getIntroSub (IChar s1 s2 s3) = s1 ++ s2 ++ s3
getIntroSub (IOrgSec s1 _ _ s2) = [s1, s2]

getStk :: StkhldrSec -> [Sentence]
getStk (StkhldrProg _ s) = [s]
getStk (StkhldrProg2 t) = concatMap getStkSub t

getStkSub :: StkhldrSub -> [Sentence]
getStkSub (Client _ s) = [s]
getStkSub Cstmr{}      = []

getGSD :: GSDSec -> [Sentence]
getGSD (GSDProg sc c cl sc1) = concatMap getSec sc ++ getCon' c
  ++ concatMap getCon' cl ++ concatMap getSec sc1
getGSD (GSDProg2 gs) = concatMap getGSDSub gs

getGSDSub :: GSDSub -> [Sentence]
getGSDSub (SysCntxt c) = concatMap getCon' c
getGSDSub (UsrChars c) = concatMap getCon' c
getGSDSub (SystCons c s) = concatMap getCon' c ++ concatMap getSec s

getScp :: ScpOfProjSec -> [Sentence]
getScp (ScpOfProjProg s c1 c2) = [s] ++ (getCon' c1) ++ (getCon' c2)

getSSD :: SSDSec -> [Sentence]
getSSD (SSDProg ssd) = concatMap getSSDSub ssd

getSSDSub :: SSDSub -> [Sentence]
getSSDSub (SSDSubVerb s)     = getSec s
getSSDSub (SSDProblem pd)    = getProblem pd
getSSDSub (SSDSolChSpec sss) = getSol sss

getProblem :: ProblemDescription -> [Sentence]
getProblem (PDProg s1 _ s2 x) = [s1]++[s2]++(concatMap getSec x)

getSol :: SolChSpec -> [Sentence]
getSol (SCSProg x) = concatMap getSCSSub x

getSCSSub :: SCSSub -> [Sentence]
getSCSSub Assumptions  = []
getSCSSub (TMs _ x)    = concatMap getTM x
getSCSSub (GDs _ x _)  = concatMap getGD x
getSCSSub (DDs s _ x _)  = s ++ concatMap getDD x
getSCSSub (IMs s _ x _)  = s ++ concatMap getIM x
getSCSSub (Constraints s1 s2 s3 lb) = [s1, s2, s3] ++ concatMap (getCon . (^. accessContents)) lb
getSCSSub (CorrSolnPpties c) = concatMap getCon' c

-- The definition of IM should not be collected because even the definition is at type
-- sentence, but the definition is not shown in Document.
getIM :: InstanceModel -> [Sentence]
getIM x = (x ^. derivations) ++ (x ^. getNotes)

getGD :: GenDefn -> [Sentence]
getGD gd = [gd ^. defn] ++ (gd ^. derivations) ++ (gd ^. getNotes)

getDD :: DataDefinition -> [Sentence]
getDD dd = (dd ^. derivations) ++ (dd ^. getNotes)

getTM :: TheoryModel -> [Sentence]
getTM x = map (^. defn) (x ^. operations) ++ map (^. defn) (x ^. defined_quant)
  ++ concatMap getTM (x ^. valid_context) ++ (x ^. getNotes)

getReq :: ReqrmntSec -> [Sentence]
getReq (ReqsProg rs) = concatMap getReqSub rs

getReqSub :: ReqsSub -> [Sentence]
getReqSub (FReqsSub c) = concatMap getCon' c
getReqSub (NonFReqsSub cc1 cc2 s1 s2) = (map (^. defn) cc1) ++ (map (^. defn) cc2)
  ++ [s1, s2]

getLcs :: LCsSec -> [Sentence]
getLcs (LCsProg c) = concatMap getCon' c

getLcs' :: LCsSec' -> [Sentence]
getLcs' (LCsProg' c) = map (^. defn) c

getUcs :: UCsSec -> [Sentence]
getUcs (UCsProg c) = concatMap getCon' c

getTrace :: TraceabilitySec -> [Sentence]
getTrace (TraceabilityProg lc s c x) = (concatMap (getCon . (^. accessContents)) lc)
  ++ s ++ concatMap getCon' c ++ concatMap getSec x

getAux :: AuxConstntSec -> [Sentence]
getAux AuxConsProg{} = []

getApp :: AppndxSec -> [Sentence]
getApp (AppndxProg c) = concatMap getCon' c

getExist :: ExistingSolnSec -> [Sentence]
getExist (ExistSolnVerb s) = getSec s
getExist (ExistSolnProg c) = concatMap getCon' c

ciGetDocDesc :: DocDesc -> [CI]
ciGetDocDesc docdesc = concatMap ciGetDocSec docdesc

ciGetDocSec :: DocSection -> [CI]
ciGetDocSec Verbatim{}              = []
ciGetDocSec RefSec{}                = []
ciGetDocSec (IntroSec        intro) = ciGetIntro intro
ciGetDocSec (StkhldrSec      stk)   = ciGetStk stk
ciGetDocSec GSDSec{}                = []
ciGetDocSec ScpOfProjSec{}          = []
ciGetDocSec (SSDSec          ssd)   = ciGetSSD ssd
ciGetDocSec ReqrmntSec{}            = []
ciGetDocSec LCsSec{}                = []
ciGetDocSec LCsSec'{}               = []
ciGetDocSec UCsSec{}                = []
ciGetDocSec TraceabilitySec{}       = []
ciGetDocSec (AuxConstntSec   aux)   = ciGetAux aux
ciGetDocSec Bibliography            = []
ciGetDocSec AppndxSec{}             = []
ciGetDocSec ExistingSolnSec{}       = []

ciGetIntro :: IntroSec -> [CI]
ciGetIntro (IntroProg _ _ insub) = concatMap ciGetIntroSub insub

ciGetIntroSub :: IntroSub -> [CI]
ciGetIntroSub IPurpose{}          = []
ciGetIntroSub IScope{}            = []
ciGetIntroSub IChar{}             = []
ciGetIntroSub (IOrgSec  _ ci _ _) = [ci]

ciGetStk :: StkhldrSec -> [CI]
ciGetStk (StkhldrProg  ci _)   = [ci]
ciGetStk (StkhldrProg2 stksub) = concatMap ciGetStkSub stksub

ciGetStkSub :: StkhldrSub -> [CI]
ciGetStkSub (Client ci1 _) = [ci1]
ciGetStkSub (Cstmr ci2)    = [ci2]

ciGetSSD :: SSDSec -> [CI]
ciGetSSD (SSDProg ssdsub) = concatMap ciGetSSDSub ssdsub

ciGetSSDSub :: SSDSub -> [CI]
ciGetSSDSub SSDSubVerb{}   = []
ciGetSSDSub (SSDProblem pd) = ciGetProbDesc pd
ciGetSSDSub SSDSolChSpec{} = []

ciGetProbDesc :: ProblemDescription -> [CI]
ciGetProbDesc (PDProg _ ci _ _) = [ci]

ciGetAux :: AuxConstntSec -> [CI]
ciGetAux (AuxConsProg ci _) = [ci]
