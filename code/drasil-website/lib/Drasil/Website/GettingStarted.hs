module Drasil.Website.GettingStarted where

import Language.Drasil

-- * Getting Started Section

gettingStartedSec :: Reference -> Reference -> Reference -> Reference -> Reference -> Reference -> Section
gettingStartedSec  quickstartWiki newWorkspaceSetupWiki contribGuideWiki workflowWiki createProjWiki debuggingWiki =
    section gettingStartedTitle -- Section title
    [mkParagraph gettingStartedIntro] -- Section introduction
    [quickStartSec quickstartWiki, newWorkspaceSec newWorkspaceSetupWiki, contribGuideWorkflowSec contribGuideWiki
    workflowWiki, createOwnProjectSec createProjWiki, debuggingDrasilSec debuggingWiki] -- Subsections
    $ makeSecRef "GettingStarted" $ S "GettingStarted" -- Section Reference

-- | Getting started section title.
gettingStartedTitle :: Sentence
gettingStartedTitle = S "Getting Started"

-- | Getting started section introduction.
gettingStartedIntro :: Sentence
gettingStartedIntro = S ""

-- | Quick Start subsection.
quickStartSec :: Reference -> Section
quickStartSec quickstartWiki =
  section quickStartTitle -- Title
  [mkParagraph $ quickStartDesc quickstartWiki] -- Contents
  [] $ makeSecRef "QuickStart" $ S "QuickStart" -- Section reference

-- | Quick Start subsection title.
quickStartTitle :: Sentence
quickStartTitle = S "Quick Start"

-- | Link to Quick Start Wiki.
quickStartDesc :: Reference -> Sentence
quickStartDesc quickstartWiki = S "Navigate to the" +:+ namedRef quickstartWiki (S "Quick Start Guide")
  +:+ S "to see what Drasil can do."

-- | New workspace subsection.
newWorkspaceSec :: Reference -> Section
newWorkspaceSec newWorkspaceSetupWiki =
  section newWorkspaceTitle -- Title
  [mkParagraph $ newWorkspaceDesc newWorkspaceSetupWiki] -- Contents
  [] $ makeSecRef "NewWorkspace" $ S "NewWorkspace" -- Section reference

-- | New workspace subsection title.
newWorkspaceTitle :: Sentence
newWorkspaceTitle = S "New Workspace Setup"

-- | Link to new workspace Wiki.
newWorkspaceDesc :: Reference -> Sentence
newWorkspaceDesc newWorkspaceSetupWiki = S "Workspace recommendations are available on the" +:+ namedRef
  newWorkspaceSetupWiki (S "New Workspace Setup") +:+ S "page."

-- | Contributor's Guide and Workflow subsection.
contribGuideWorkflowSec :: Reference -> Reference -> Section
contribGuideWorkflowSec contribGuideWiki workflowWiki =
  section contribGuideWorkflowTitle -- Title
  [mkParagraph $ contribGuideWorkflowDesc contribGuideWiki workflowWiki] -- Contents
  [] $ makeSecRef "ContribGuideWorkflow" $ S "ContribGuideWorkflow" -- Section reference

-- | Contributor's Guide and Workflow title.
contribGuideWorkflowTitle :: Sentence
contribGuideWorkflowTitle = S "Contributor's Guide and Workflow"

-- | Link to Contributor's Guide and Workflow Wiki.
contribGuideWorkflowDesc :: Reference -> Reference -> Sentence
contribGuideWorkflowDesc contribGuideWiki workflowWiki = S "If you are interested in contributing to the \
  \project, please look at the" +:+ namedRef contribGuideWiki (S "Contributor's Guide") +:+ S" as well as the"
  +:+ namedRef workflowWiki (S "Workflow") +:+ S "page."

-- | Creating Your Own Project subsection.
createOwnProjectSec :: Reference -> Section
createOwnProjectSec createProjWiki =
  section createOwnProjectTitle -- Title
  [mkParagraph $ createOwnProjectDesc createProjWiki] -- Contents
  [] $ makeSecRef "OwnProject" $ S "OwnProject" -- Section reference

-- | Creating Your Own Project title.
createOwnProjectTitle :: Sentence
createOwnProjectTitle = S "Creating Your Own Project"

-- | Link to Creating Your Own Project Wiki.
createOwnProjectDesc :: Reference -> Sentence
createOwnProjectDesc createProjWiki = S "If you are interested in creating your own project in Drasil, \
  \please look at the" +:+ namedRef createProjWiki (S "Creating Your Project in Drasil") +:+ S "page."

-- | Debugging Drasil subsection.
debuggingDrasilSec :: Reference -> Section
debuggingDrasilSec debuggingWiki =
  section debuggingDrasilTitle -- Title
  [mkParagraph $ debuggingDrasilDesc debuggingWiki] -- Contents
  [] $ makeSecRef "DebuggingDrasil" $ S "DebuggingDrasil" -- Section reference

-- | Debugging Drasil title.
debuggingDrasilTitle :: Sentence
debuggingDrasilTitle = S "Debugging Drasil"

-- | Debugging Drasil Wiki.
debuggingDrasilDesc :: Reference -> Sentence
debuggingDrasilDesc debuggingWiki = S "Debugging information can be found on the" +:+ namedRef
  debuggingWiki (S "Debugging in Drasil") +:+ S "page."
