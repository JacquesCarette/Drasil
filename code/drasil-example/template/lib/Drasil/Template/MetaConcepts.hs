-- Changes to this template should be reflected in the 'Creating Your Project
-- in Drasil' tutorial found on the wiki:
-- https://github.com/JacquesCarette/Drasil/wiki/Creating-Your-Project-in-Drasil
-- This comment can be removed after copying this template to build your own example.

module Drasil.Template.MetaConcepts (progName, projName) where

import Drasil.Database (mkUid)
import Language.Drasil
import Drasil.System (ProjectName, mkCommonProjName)

-- MOVE TO CONCEPTS
progName :: CI -- FIXME: Replace "template" with the name of your project!
progName = commonIdea (mkUid "templateName") (pn "Template") "Template" []

projName :: ProjectName
projName = mkCommonProjName (mkUid "templateProjName") (nounPhraseSP "Template") "Template"
