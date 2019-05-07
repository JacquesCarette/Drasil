module Drasil.DocLang (
    -- DocumentLanguage
    AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), DocDesc, 
    DocSection(..), Emphasis(..), GSDSec(GSDProg2), GSDSub(UsrChars, SystCons, SysCntxt), 
    IntroSec(..), IntroSub(..), LCsSec(..), LFunc(..), 
    Literature(Doc', Lit, Manual), ProblemDescription(..), RefSec(..), RefTab(..), 
    ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub, NonFReqsSub'), ScpOfProjSec(ScpOfProjProg), 
    SCSSub(..), SSDSec(..), SSDSub(..), SolChSpec(..), ExistingSolnSec(..),
    StkhldrSec(StkhldrProg2), LCsSec'(..),
    StkhldrSub(Client, Cstmr), TConvention(..), TraceabilitySec(TraceabilityProg), 
    TSIntro(..), UCsSec(..), mkDoc,
    tsymb, tsymb'', mkEnumSimple, mkEnumSimpleD, mkListTuple,
    -- DocumentLanguage.Definitions
    Field(..), Fields, InclUnits(IncludeUnits), Verbosity(Verbose), ddefn,
    -- DocumentLanguage.RefHelpers 
    ModelDB, ddRefDB, mdb,
    -- DocumentLanguage.TraceabilityMatrix
    -- Sections.AuxiliaryConstants
    valsOfAuxConstantsF,
    -- Sections.GeneralSystDesc
    genSysF, 
    -- Sections.Introduction
    -- Sections.ReferenceMaterial
    intro,
    -- Sections.Requirements
    nonFuncReqF, reqF,
    -- Sections.ScopeOfTheProject
    -- Sections.SolutionCharacterSpec
    SubSec, assembler, sSubSec, siCon, siDDef, siIMod, siSTitl, siSent, siTMod, 
    siUQI, siUQO,
    -- Sections.SpecificSystemDescription
    assumpF, dataConstraintUncertainty, dataDefnF, goalStmtF, inDataConstTbl, 
    inModelF, outDataConstTbl, physSystDesc, probDescF, termDefnF, specSysDescr,
    -- Sections.Stakeholders
    -- Sections.TableOfAbbAndAcronyms
    -- Sections.TableOfSymbols
    -- Sections.TableOfUnits
    -- Sections.TraceabilityMandGs
    traceGIntro, traceMGF, generateTraceTable,
    -- ExtractDocDesc
    getDocDesc, egetDocDesc, ciGetDocDesc, generateTraceMap,
    -- Tracetable
    getTraceMapFromTM, getTraceMapFromGD,
    getTraceMapFromDD, getTraceMapFromIM, getSCSSub, generateTraceMap',
   -- Labels
    goalStmtLabel, solutionLabel, characteristicsLabel,
    physSystDescriptionLabel
    ) where 

import Drasil.DocumentLanguage (AppndxSec(..), AuxConstntSec(..), 
    DerivationDisplay(..), DocDesc, DocSection(..), Emphasis(..), ExistingSolnSec(..), 
    GSDSec(GSDProg2), GSDSub(UsrChars, SystCons, SysCntxt), IntroSec(..), IntroSub(..), 
    LCsSec(..), LFunc(..), Literature(Doc', Lit, Manual), ProblemDescription(..), 
    RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub, NonFReqsSub'), 
    ScpOfProjSec(ScpOfProjProg), SCSSub(..), SSDSec(..), SSDSub(..), SolChSpec(..), 
    StkhldrSec(StkhldrProg2), StkhldrSub(Client, Cstmr), TConvention(..), LCsSec'(..),
    TraceabilitySec(TraceabilityProg), TSIntro(..), UCsSec(..), mkDoc, tsymb, tsymb'',
    mkEnumSimple, mkEnumSimpleD, mkListTuple)
import Drasil.DocumentLanguage.Definitions (Field(..), Fields, 
    InclUnits(IncludeUnits), Verbosity(Verbose), ddefn)
import Drasil.DocumentLanguage.RefHelpers (ModelDB, ddRefDB, mdb)
--import Drasil.DocumentLanguage.TraceabilityMatrix
import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)
import Drasil.Sections.GeneralSystDesc (genSysF)
--import Drasil.Sections.Introduction
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements (nonFuncReqF, reqF)
--import Drasil.Sections.ScopeOfTheProject
import Drasil.Sections.SolutionCharacterSpec (SubSec, assembler, sSubSec, siCon, 
    siDDef, siIMod, siSTitl, siSent, siTMod, siUQI, siUQO)
import Drasil.Sections.SpecificSystemDescription (assumpF, 
    dataConstraintUncertainty, dataDefnF, goalStmtF, inDataConstTbl, inModelF, 
    outDataConstTbl, physSystDesc, probDescF, termDefnF, specSysDescr)
--import Drasil.Sections.Stakeholders
--import Drasil.Sections.TableOfAbbAndAcronyms
--import Drasil.Sections.TableOfSymbols
--import Drasil.Sections.TableOfUnits
import Drasil.Sections.TraceabilityMandGs (traceGIntro, traceMGF, generateTraceTable)
import Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, ciGetDocDesc)
import Drasil.TraceTable (generateTraceMap, getTraceMapFromTM, getTraceMapFromGD,
    getTraceMapFromDD, getTraceMapFromIM, getSCSSub, generateTraceMap')
-- Commented out modules aren't used - uncomment if this changes
import Drasil.DocumentLanguage.Labels (goalStmtLabel, solutionLabel, characteristicsLabel,
    physSystDescriptionLabel)
