module Drasil.ExtractDocDesc (getDoc, egetDoc) where

import Control.Lens(makeLenses, (^.), view)
import Drasil.DocumentLanguage
import Language.Drasil hiding (Manual, Vector, Verb)


egetDocDesc :: DocDesc -> [Expr]
egetDocDesc d = concatMap egetDocSec d


egetDocSec :: DocSection -> [Expr]
egetDocSec (Verbatim a)         = egetSec a
egetDocSec (RefSec r)           = egetRefSec r
egetDocSec (IntroSec i)         = []
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
egetAux (AuxConsProg c qd) = concatMap egetQDef qd

egetApp :: AppndxSec -> [Expr]
egetApp (AppndxProg c) = concatMap egetCon' c

egetExist :: ExistingSolnSec -> [Expr]
egetExist (ExistSolnVerb s) = egetSec s
egetExist (ExistSolnProg c) = concatMap egetCon' c

egetRefProg :: RefTab -> [Expr]
egetRefProg (TUnits) = []
egetRefProg (TUnits' _) = []
egetRefProg (TSymb t)   = []
egetRefProg (TSymb' l t) = egetFunc l
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
egetFunc (TermExcept dqd) = []
egetFunc (DefnExcept dqd) = []
egetFunc (TAD) = []

egetProblem :: ProblemDescription -> [Expr]
egetProblem (PDProg _ _ _ s) = concatMap egetSec s

egetSol :: SolChSpec -> [Expr]
egetSol (SCSProg s) = concatMap egetSCSSub s

egetSCSSub :: SCSSub -> [Expr]
egetSCSSub (Assumptions) = []
egetSCSSub (TMs _ tm)    = concatMap egetTM tm
egetSCSSub (GDs _ gd _)  = []
egetSCSSub (DDs _ dd _)  = concatMap egetDD dd
egetSCSSub (IMs _ im _)  = []
egetSCSSub (Constraints _ _ _ lc) = concatMap egetLblCon lc
egetSCSSub (CorrSolnPpties c) = concatMap egetCon' c

egetTM :: TheoryModel -> [Expr]
egetTM tm = concatMap egetQDef (tm ^. defined_quant ++ tm ^. defined_fun)
  ++ concatMap egetTheoryChunk (tm ^. valid_context)

egetTheoryChunk :: TheoryChunk -> [Expr]
egetTheoryChunk tc = concatMap egetTheoryChunk (tc ^. valid_context) ++
  concatMap egetQDef (tm ^. defined_quant ++ tm ^. defined_fun)

egetDD :: DataDefinition -> [Expr]
egetDD dd = [dd ^. defnExpr]

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
getDocSec (TraceabilitySec t)  = getTrace t  -- Stops here
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
getTuIntro (System) = [System]
getTuIntro (_) = []

getTsIntro :: TSIntro -> [Sentence]
getTsIntro (TypogConvention tc) = concatMap getTConv tc
getTsIntro (_) = []

{--data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc
  TAD :: LFunc
  Fixme!!--}
getLFunc :: LFunc -> [Sentence]
getLFunc (_) = []

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
getGSD (GSDProg2 gs) = getGSDSub gs

getGSDSub :: GSDSub -> [Sentence]
getGSDSub (SysCntxt c) = concatMap getCon' c
getGSDSub (UsrChars c) = concatMap getCon' c
getGSDSub (SystCons c s) = concatMap getCon' c ++ concatMap getSec s

getScp :: ScpOfProjSec -> [Sentence]
getScp (ScpOfProjProg s c1 c2) = s:(getCon' c1):(getCon' c2)

getSSD :: SSDSec -> [Sentence]
getSSD (SSDProg ssd) = concatMap getSSDSub ssd

getSSDSub :: SSDSub -> [Sentence]
getSSDSub (SSDSubVerb s)     = getSec s
getSSDSub (SSDProblem pd)    = getProblem pd
getSSDSub (SSDSolChSpec sss) = getSol sss

getProblem :: ProblemDescription -> [Sentence]
getProblem (PDProg s1 _ s2 sec) = s1:[s2]:(concatMap getsec sec)

getSol :: SolChSpec -> [Sentence]
getSol (SCSProg sub) = concatMap getSCSSub sub

getSCSSub :: SCSSub -> [Sentence]
getSCSSub (Assumptions) = []
getSCSSub (TMs _ tm)    = concatMap getTM tm
getSCSSub (GDs _ gd _)  = concatMap getGD gd
getSCSSub (DDs _ dd _)  = concatMap getDD dd
getSCSSub (IMs _ im _)  = concatMap getIM im
getSCSSub (Constraints s1 s2 s3 lb) = s1:[s2]:[s3]:(concatMap getCon (concatMap (^. accessContents) lb))
getSCSSub (CorrSolnPpties c) = concatMap getCon' c

getIM :: InstanceModel -> [Sentence]
getIM im = (im ^. getReferences) ++ (im ^. derivations) ++ (im ^. getNotes) ++
  [im ^. relat] ++ [im ^. defn]

getGD :: GenDefn -> [Sentence]
getGD gd = [im ^. relat] ++ [im ^. defn] ++ (im ^. derivations) ++ (im ^. getReferences)
  ++ (im ^. getNotes)

getDD :: DataDefinition -> [Sentence]
getDD dd = (dd ^. derivations) ++ (dd ^. getReferences) ++ (dd ^. getNotes)

getTM :: TheoryModel -> [Sentence]
getTM tm = (dd ^. getReferences) ++ (dd ^. getNotes) ++ [im ^. defn]

getReq :: ReqrmntSec -> [Sentence]
getReq (ReqsProg rs) = concatMap getReqSub rs

getReqSub :: ReqsSub -> [Sentence]
getReqSub (FReqsSub c) = concatMap getCon' c
getReqSub (NonFReqsSub cc1 cc2 s1 s2) = (concatMap (^. defn) cc1) ++ (concatMap (^. defn) cc2)
  ++ [s1] ++ [s2]

getLcs :: LCsSec -> [Sentence]
getLcs (LCsProg c) = concatMap getCon' c

getUcs :: UCsSec -> [Sentence]
getUcs (UCsProg c) = concatMap getCon' c













