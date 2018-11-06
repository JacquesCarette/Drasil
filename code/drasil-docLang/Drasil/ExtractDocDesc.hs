module Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, ciGetDocDesc) where

import Control.Lens((^.))
import Drasil.DocumentLanguage
import Language.Drasil hiding (Manual, Vector, Verb)


egetDocDesc :: DocDesc -> [Expr]
egetDocDesc d = concatMap egetDocSec d


egetDocSec :: DocSection -> [Expr]
egetDocSec (Verbatim a)         = egetSec a
egetDocSec (RefSec r)           = egetRefSec r
egetDocSec (IntroSec _)         = []
egetDocSec (StkhldrSec s)       = egetStk s
egetDocSec (GSDSec g)           = egetGSD g
egetDocSec (ScpOfProjSec s)     = egetScp s
egetDocSec (SSDSec s)           = egetSSD s
egetDocSec (ReqrmntSec r)       = egetReq r
egetDocSec (LCsSec l)           = egetLcs l
egetDocSec (UCsSec u)           = egetUcs u
egetDocSec (TraceabilitySec t)  = egetTrace t
egetDocSec (AuxConstntSec a)    = egetAux a
egetDocSec (Bibliography)       = []
egetDocSec (AppndxSec a)        = egetApp a
egetDocSec (ExistingSolnSec e)  = egetExist e


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

egetApp :: AppndxSec -> [Expr]
egetApp (AppndxProg c) = concatMap egetCon' c

egetExist :: ExistingSolnSec -> [Expr]
egetExist (ExistSolnVerb s) = egetSec s
egetExist (ExistSolnProg c) = concatMap egetCon' c

egetRefProg :: RefTab -> [Expr]
egetRefProg (TUnits) = []
egetRefProg (TUnits' _) = []
egetRefProg (TSymb _)   = []
egetRefProg (TSymb' l _) = egetFunc l
egetRefProg (TAandA) = []

egetStk :: StkhldrSec -> [Expr]
egetStk (StkhldrProg _ _) = []
egetStk (StkhldrProg2 s)  = concatMap egetStkSub s

egetStkSub :: StkhldrSub -> [Expr]
egetStkSub (_) = []

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
egetReqSub (_) = []

egetFunc :: LFunc -> [Expr]
egetFunc (Term) = []
egetFunc (Defn) = []
egetFunc (TermExcept _) = []
egetFunc (DefnExcept _) = []
egetFunc (TAD) = []

egetProblem :: ProblemDescription -> [Expr]
egetProblem (PDProg _ _ _ s) = concatMap egetSec s

egetSol :: SolChSpec -> [Expr]
egetSol (SCSProg s) = concatMap egetSCSSub s

egetSCSSub :: SCSSub -> [Expr]
egetSCSSub (Assumptions) = []
egetSCSSub (TMs _ x)    = concatMap egetTM x
egetSCSSub (GDs _ x _)  = concatMap egetGD x
egetSCSSub (DDs _ x _)  = concatMap egetDD x
egetSCSSub (IMs _ x _)  = concatMap egetIM x
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
getDocSec (UCsSec u)           = getUcs u
getDocSec (TraceabilitySec t)  = getTrace t
getDocSec (AuxConstntSec a)    = getAux a
getDocSec (Bibliography)       = []
getDocSec (AppndxSec a)        = getApp a
getDocSec (ExistingSolnSec e)  = getExist e

getRefSec :: RefSec -> [Sentence]
getRefSec (RefProg c r) = getCon' c ++ concatMap getReftab r

getReftab :: RefTab -> [Sentence]
getReftab (TUnits) = []
getReftab (TUnits' tu) = concatMap getTuIntro tu
getReftab (TSymb ts) = concatMap getTsIntro ts
getReftab (TSymb' lf ts) = getLFunc lf ++ concatMap getTsIntro ts
getReftab (TAandA) = []

getTuIntro :: TUIntro -> [Sentence]
getTuIntro (_) = []

getTsIntro :: TSIntro -> [Sentence]
getTsIntro (TypogConvention tc) = concatMap getTConv tc
getTsIntro (_) = []

getLFunc :: LFunc -> [Sentence]
getLFunc (Term) = []
getLFunc (Defn) = []
getLFunc (TermExcept x) = map (^. defn) x
getLFunc (DefnExcept x) = map (^. defn) x
getLFunc (TAD)  = []

getTConv :: TConvention -> [Sentence]
getTConv (Vector _) = []
getTConv (Verb s) = [s]

getIntrosec :: IntroSec -> [Sentence]
getIntrosec (IntroProg s1 s2 is) = [s1] ++ [s2] ++ concatMap getIntroSub is

getIntroSub :: IntroSub -> [Sentence]
getIntroSub (IPurpose s) = [s]
getIntroSub (IScope s1 s2) = [s1] ++ [s2]
getIntroSub (IChar s1 s2 s3) = [s1] ++ [s2] ++ [s3]
getIntroSub (IOrgSec s1 _ _ s2) = [s1] ++ [s2]

getStk :: StkhldrSec -> [Sentence]
getStk (StkhldrProg _ s) = [s]
getStk (StkhldrProg2 t) = concatMap getStkSub t

getStkSub :: StkhldrSub -> [Sentence]
getStkSub (Client _ s) = [s]
getStkSub (Cstmr _)    = []

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
getSCSSub (Assumptions) = []
getSCSSub (TMs _ x)    = concatMap getTM x
getSCSSub (GDs _ x _)  = concatMap getGD x
getSCSSub (DDs _ x _)  = concatMap getDD x
getSCSSub (IMs _ x _)  = concatMap getIM x
getSCSSub (Constraints s1 s2 s3 lb) = [s1]++[s2]++[s3]++(concatMap getCon (map (^. accessContents) lb))
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
  ++ [s1] ++ [s2]

getLcs :: LCsSec -> [Sentence]
getLcs (LCsProg c) = concatMap getCon' c

getUcs :: UCsSec -> [Sentence]
getUcs (UCsProg c) = concatMap getCon' c

getTrace :: TraceabilitySec -> [Sentence]
getTrace (TraceabilityProg lc s c x) = (concatMap getCon (map (^. accessContents) lc))
  ++ s ++ concatMap getCon' c ++ concatMap getSec x

getAux :: AuxConstntSec -> [Sentence]
getAux (AuxConsProg _ _) = []

getApp :: AppndxSec -> [Sentence]
getApp (AppndxProg c) = concatMap getCon' c

getExist :: ExistingSolnSec -> [Sentence]
getExist (ExistSolnVerb s) = getSec s
getExist (ExistSolnProg c) = concatMap getCon' c

ciGetDocDesc :: DocDesc -> [CI]
ciGetDocDesc docdesc = concatMap ciGetDocSec docdesc

ciGetDocSec :: DocSection -> [CI]
ciGetDocSec (Verbatim        sec)     = []
ciGetDocSec (RefSec          refsec)  = []
ciGetDocSec (IntroSec        intro)   = ciGetIntro intro
ciGetDocSec (StkhldrSec      stk)     = ciGetStk stk
ciGetDocSec (GSDSec          gsd)     = []
ciGetDocSec (ScpOfProjSec    scpPro)  = []
ciGetDocSec (SSDSec          ssd)     = ciGetSSD ssd
ciGetDocSec (ReqrmntSec      req)     = []
ciGetDocSec (LCsSec          lc)      = []
ciGetDocSec (UCsSec          uc)      = []
ciGetDocSec (TraceabilitySec trace)   = []
ciGetDocSec (AuxConstntSec   aux)     = ciGetAux aux
ciGetDocSec (Bibliography)            = []
ciGetDocSec (AppndxSec       app)     = []
ciGetDocSec (ExistingSolnSec exist)   = []

ciGetIntro :: IntroSec -> [CI]
ciGetIntro (IntroProg _ _ insub) = concatMap ciGetIntroSub insub

ciGetIntroSub :: IntroSub -> [CI]
ciGetIntroSub (IPurpose _)        = []
ciGetIntroSub (IScope   _ _)      = []
ciGetIntroSub (IChar    _ _ _)    = []
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
ciGetSSDSub (SSDSubVerb _)         = []
ciGetSSDSub (SSDProblem pd)        = ciGetProbDesc pd
ciGetSSDSub (SSDSolChSpec solspec) = []

ciGetProbDesc :: ProblemDescription -> [CI]
ciGetProbDesc (PDProg _ ci _ _) = [ci]

ciGetAux :: AuxConstntSec -> [CI]
ciGetAux (AuxConsProg ci _) = [ci]













