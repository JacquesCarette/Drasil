module Drasil.DocLang (
    -- DocumentLanguage
    DocSection(RefSec, Verbatim), 
    Literature(Lit, Manual), RefSec(..), RefTab(TUnits), 
    TSIntro(SymbConvention, TSPurpose), DocDesc, mkDoc, tsymb,
    -- Sections.ReferenceMaterial
    intro,
    -- Sections.SpecificSystemDescription
    dataDefnF
    ) where 

import Drasil.DocumentLanguage (DocSection(RefSec, Verbatim), 
    Literature(Lit, Manual), RefSec(..), RefTab(TUnits), 
    TSIntro(SymbConvention, TSPurpose), DocDesc, mkDoc, tsymb)
import Drasil.DocumentLanguage.Definitions
import Drasil.DocumentLanguage.RefHelpers
import Drasil.DocumentLanguage.TraceabilityMatrix
import Drasil.Sections.AuxiliaryConstants
import Drasil.Sections.GeneralSystDesc
import Drasil.Sections.Introduction
import Drasil.Sections.ReferenceMaterial (intro)
import Drasil.Sections.Requirements
import Drasil.Sections.ScopeOfTheProject
import Drasil.Sections.SolutionCharacterSpec
import Drasil.Sections.SpecificSystemDescription (dataDefnF)
import Drasil.Sections.Stakeholders
import Drasil.Sections.TableOfAbbAndAcronyms
import Drasil.Sections.TableOfSymbols
import Drasil.Sections.TableOfUnits
import Drasil.Sections.TraceabilityMandGs
import Drasil.SRS hiding (intro) -- FIXME: intro might be used in drasil-example