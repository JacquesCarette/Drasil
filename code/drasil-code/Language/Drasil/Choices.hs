module Language.Drasil.Choices (
  Choices(..), Modularity(..), InputModule(..), inputModule, Structure(..), 
  ConstantStructure(..), ConstantRepr(..), MatchedConceptMap, CodeConcept(..), 
  matchConcepts, SpaceMatch, MatchedSpaces, matchSpaces, ImplementationType(..),
  ConstraintBehaviour(..), Comments(..), Verbosity(..), Visibility(..), 
  Logging(..), AuxFile(..), getSampleData, hasSampleInput, hasReadMe, defaultChoices
) where

import Language.Drasil

import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg)

import GOOL.Drasil (CodeType)

import Control.Lens ((^.))
import Data.Map (Map, fromList)

data Choices = Choices {
-- Global design choices (affect entire program)
  -- Target languages
  -- Choosing multiple means program will be generated in multiple languages
  lang :: [Lang],
  -- How the program should be modularized
  modularity :: Modularity,
  -- Structure of inputs (bundled or not)
  inputStructure :: Structure,
  -- Structure of constants (inlined or bundled or not, or stored with inputs)
  constStructure :: ConstantStructure,
  -- Representation of constants (as variables or as constants)
  constRepr :: ConstantRepr,
  -- Map of UIDs for Drasil concepts to code concepts
  -- Matching a UID to a code concept means the code concept should be used
  -- instead of the chunk associated with the UID
  conceptMatch :: ConceptMatchMap,
  -- Map of Spaces to CodeTypes
  -- Matching a Space to a CodeType means values of the Space should have that
  -- CodeType in the generated code.
  spaceMatch :: SpaceMatch,
-- Local design choices (affect one part of program)
  -- Implementation type, program or library
  impType :: ImplementationType,
  -- Preferentially-ordered list ODE libraries to try
  odeLib :: [ODELibPckg],
  -- FIXME: ODEInfos should be automatically built from Instance models when 
  -- needed, but we can't do that yet so I'm passing it through Choices instead.
  -- This choice should really just be for an ODEMethod
  odes :: [ODEInfo],
  -- Constraint violation behaviour. Exception or Warning.
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour,
-- Features that can be toggled on on off
  -- Turns Doxygen comments for different code structures on or off
  comments :: [Comments],
  -- Standard output from running Doxygen: verbose or quiet?
  doxVerbosity :: Verbosity,
  -- Turns date field on or off in the generated module-level Doxygen comments
  dates :: Visibility,
  -- Turns different forms of logging on or off
  logging :: [Logging],
  -- Name of log file
  logFile :: FilePath,
  -- Turns generation of different auxiliary (non-source-code) files on or off
  auxFiles :: [AuxFile]
}

data Modularity = Modular InputModule | Unmodular

data InputModule = Combined -- Input-related functions combined in one module
                 | Separated -- Input-related functions each in own module  

inputModule :: Choices -> InputModule
inputModule c = inputModule' $ modularity c
  where inputModule' Unmodular = Combined
        inputModule' (Modular im) = im
                 
data Structure = Unbundled -- Individual variables
               | Bundled -- Variables bundled in a class

data ConstantStructure = Inline -- Inline values for constants
                       | WithInputs -- Store constants with inputs
                       | Store Structure -- Store constants separately from 
                                         -- inputs, whether bundled or unbundled

data ConstantRepr = Var | Const

type ConceptMatchMap = Map UID [CodeConcept]
type MatchedConceptMap = Map UID CodeConcept

-- Currently we only support one code concept, more will be added later
data CodeConcept = Pi

matchConcepts :: (HasUID c) => [(c, [CodeConcept])] -> ConceptMatchMap
matchConcepts = fromList . map (\(cnc,cdc) -> (cnc ^. uid, cdc))

type SpaceMatch = Space -> [CodeType]
type MatchedSpaces = Space -> CodeType

matchSpace :: Space -> [CodeType] -> SpaceMatch -> SpaceMatch
matchSpace _ [] _ = error "Must match each Space to at least one CodeType"
matchSpace s ts sm = \sp -> if sp == s then ts else sm sp

matchSpaces :: [(Space, [CodeType])] -> SpaceMatch
matchSpaces spMtchs = matchSpaces' spMtchs spaceToCodeType 
  where matchSpaces' ((s,ct):sms) sm = matchSpaces' sms $ matchSpace s ct sm
        matchSpaces' [] sm = sm

data ImplementationType = Library
                        | Program
                        
data ConstraintBehaviour = Warning -- Print warning when constraint violated
                         | Exception -- Throw exception when constraint violated
        
data Comments = CommentFunc -- Function/method-level comments
              | CommentClass -- class-level comments
              | CommentMod -- File/Module-level comments
              deriving Eq

data Verbosity = Verbose | Quiet

data Visibility = Show
                | Hide

-- Eq instances required for Logging and Comments because generator needs to 
-- check membership of these elements in lists
data Logging = LogFunc -- Log messages generated for function calls
             | LogVar -- Log messages generated for variable assignments
             deriving Eq

-- Currently we only support one kind of auxiliary file: sample input file
-- To generate a sample input file compatible with the generated program
-- FilePath is the path to the user-provided file containing a sample set of input data
data AuxFile = SampleInput FilePath 
                | ReadME 
                deriving (Eq , Show)

getSampleData :: Choices -> Maybe FilePath
getSampleData chs = getSampleData' (auxFiles chs)
  where getSampleData' (SampleInput fp:_) = Just fp
        getSampleData' _ = Nothing

hasSampleInput :: [AuxFile] -> Bool
hasSampleInput [] = False
hasSampleInput (SampleInput _:_) = True
hasSampleInput (_:xs) = hasSampleInput xs

hasReadMe :: [AuxFile] -> Bool
hasReadMe [] = False
hasReadMe (ReadME:_) = True
hasReadMe (_:xs) = hasReadMe xs

defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  modularity = Modular Combined,
  impType = Program,
  logFile = "log.txt",
  logging = [],
  comments = [],
  doxVerbosity = Verbose,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Warning,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  conceptMatch = matchConcepts ([] :: [(QDefinition, [CodeConcept])]),
  spaceMatch = spaceToCodeType, 
  auxFiles = [ReadME],
  odeLib = [],
  odes = []
}