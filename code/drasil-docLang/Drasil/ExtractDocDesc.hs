module Drasil.ExtractDocDesc (getDoc, egetDoc) where

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

{--
data StkhldrSec = StkhldrProg CI Sentence | StkhldrProg2 [StkhldrSub]

data StkhldrSub where
  Client :: (Idea a) => a -> Sentence -> StkhldrSub
  Cstmr  :: (Idea a) => a -> StkhldrSub--}
egetStk :: StkhldrSec -> [Expr]
egetStk (_) = []

egetGSDSub :: GSDSub -> [Expr]
egetGSDSub (SysCntxt c)   = concatMap egetCon' c
egetGSDSub (UsrChars c)   = concatMap egetCon' c
egetGSDSub (SystCons c s) = concatMap egetCon' c ++ concatMap egetSec s

egetSSDSub :: SSDSub -> [Expr]
egetSSDSub (SSDSubVerb s)   = egetSec s
egetSSDSub (SSDProblem p)   = egetProblem p
egetSSDSub (SSDSolChSpec s) = egetSol s

{--data ReqsSub where
  FReqsSub :: [Contents] -> ReqsSub --FIXME: Should be ReqChunks?
  NonFReqsSub :: (Concept c) => [c] -> [c] -> Sentence -> Sentence -> ReqsSub
  Fixme! --}
egetReqSub :: ReqsSub -> [Expr]
egetReqSub (FReqsSub c) = concatMap egetCon' c
egetReqSub (_) = []

{--data LFunc where
  Term :: LFunc
  Defn :: LFunc
  TermExcept :: Concept c => [c] -> LFunc
  DefnExcept :: Concept c => [c] -> LFunc
  TAD :: LFunc
  Fixme! --}
egetFunc :: LFunc -> [Expr]
egetFunc (_) = []

{--data ProblemDescription where
  PDProg :: (Idea a) => Sentence -> a -> Sentence -> [Section] -> ProblemDescription
  Fixme!--}
egetProblem :: ProblemDescription -> [Expr]
egetProblem (_) = []

{--data SolChSpec where
  SCSProg :: [SCSSub] -> SolChSpec
  --}
egetSol :: SolChSpec -> [Expr]
egetSol (SCSProg s) = concatMap egetSCSSub s


egetSCSSub :: SCSSub -> [Expr]
egetSCSSub (Assumptions) = []
egetSCSSub (_)    = [] --concatMap egetTM tm
egetSCSSub (Constraints _ _ _ lc) = concatMap egetLblCon lc
egetSCSSub (CorrSolnPpties c) = concatMap egetCon' c





