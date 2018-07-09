module Drasil.DocLang (
    -- DocumentLanguage
    AuxConstntSec (AuxConsProg), DocDesc, 
    DocSection (AuxConstntSec, Bibliography, IntroSec, RefSec, Verbatim), 
    LFunc (TermExcept), Literature (Doc', Lit, Manual), IntroSec (IntroProg), 
    IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (..), 
    RefTab (TAandA, TUnits), TSIntro (SymbConvention, SymbOrder, TSPurpose),
    mkDoc, mkLklyChnk, mkRequirement, mkUnLklyChnk, tsymb, tsymb'',
    -- DocumentLanguage.RefHelpers 
    ModelDB, mdb, refA, refDD, ddRefDB,
    -- Sections.ReferenceMaterial
    intro,
    -- Sections.Requirements (
    nonFuncReqF,
    -- Sections.SpecificSystemDescription
    dataDefnF,
    ) where 

import Drasil.DocumentLanguage (AuxConstntSec (AuxConsProg), DocDesc, 
    DocSection (AuxConstntSec, Bibliography, IntroSec, RefSec, Verbatim), 
    LFunc (TermExcept), Literature (Doc', Lit, Manual), IntroSec (IntroProg), 
    IntroSub(IChar, IOrgSec, IPurpose, IScope), RefSec (..), 
    RefTab (TAandA, TUnits), TSIntro (SymbConvention, SymbOrder, TSPurpose),
    mkDoc, mkLklyChnk, mkRequirement, mkUnLklyChnk, tsymb, tsymb'')
import Drasil.DocumentLanguage.Definitions
import Drasil.DocumentLanguage.RefHelpers (ModelDB, mdb, refA, refDD, ddRefDB)
import Drasil.DocumentLanguage.TraceabilityMatrix
import Drasil.Sections.AuxiliaryConstants
import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.Introduction
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements (nonFuncReqF)
import Drasil.Sections.ScopeOfTheProject
import Drasil.Sections.SolutionCharacterSpec
import Drasil.Sections.SpecificSystemDescription (dataDefnF)
import Drasil.Sections.Stakeholders
import Drasil.Sections.TableOfAbbAndAcronyms
import Drasil.Sections.TableOfSymbols
import Drasil.Sections.TableOfUnits
import Drasil.Sections.TraceabilityMandGs
import Drasil.SRS hiding (intro)-- FIXME: intro might be used in drasil-example