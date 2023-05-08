-- | Defines the design language for SCS.
module Language.Drasil.Choices (
  Choices(..), Architecture (..), makeArchit, DataInfo(..), makeData, Maps(..), 
  makeMaps, spaceToCodeType, Constraints(..), makeConstraints, ODE(..), makeODE, 
  DocConfig(..), makeDocConfig, LogConfig(..), makeLogConfig, OptionalFeatures(..), 
  makeOptFeats, ExtLib(..), Modularity(..), InputModule(..), inputModule, Structure(..),
  ConstantStructure(..), ConstantRepr(..), ConceptMatchMap, MatchedConceptMap,
  CodeConcept(..), matchConcepts, SpaceMatch, matchSpaces, ImplementationType(..),
  ConstraintBehaviour(..), Comments(..), Verbosity(..), Visibility(..),
  Logging(..), AuxFile(..), getSampleData, hasSampleInput, defaultChoices,
  choicesSent, showChs) where

import Language.Drasil hiding (None, Var)
import Language.Drasil.Code.Code (spaceToCodeType)
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg)

import GOOL.Drasil (CodeType)

import Control.Lens ((^.))
import Data.Map (Map, fromList)

-- | The instruction indicates how the generated program should be written down.
-- Full details of Choices documentation https://github.com/JacquesCarette/Drasil/wiki/The-Code-Generator
data Choices = Choices {
  -- | Target languages.
  -- Choosing multiple means program will be generated in multiple languages.
  lang :: [Lang],
  -- | Architecture of the program, include modularity and implementation type
  architecture :: Architecture,
  -- | Data structure and represent
  dataInfo :: DataInfo,
  -- | Maps for 'Drasil concepts' to 'code concepts' or 'Space' to a 'CodeType
  maps :: Maps,
  -- | Setting for Softifacts that can be added to the program or left it out
  optFeats :: OptionalFeatures,
  -- | Constraint violation behaviour. Exception or Warning.
  srsConstraints :: Constraints,
  -- | List of external libraries what to utilize 
  extLibs :: [ExtLib]
}

-- | Renders program choices as a 'Sentence'.
class RenderChoices a where
    showChs :: a -> Sentence
    showChsList :: [a] -> Sentence
    showChsList lst = foldlSent_ (map showChs lst)

-- | Architecture of a program
data Architecture = Archt {
  -- | How the program should be modularized.
  modularity :: Modularity,
  -- | Implementation type, program or library.
  impType :: ImplementationType
}
-- | Constructor to create a Architecture
makeArchit :: Modularity -> ImplementationType -> Architecture
makeArchit = Archt

-- | Modularity of a program.
data Modularity = Modular InputModule -- ^ Different modules. For controller, 
                                      -- input, calculations, output.
                | Unmodular -- ^ All generated code is in one module/file.

-- | Renders the modularity of a program.
instance RenderChoices Modularity where
  showChs Unmodular = S "Unmodular"
  showChs (Modular Combined) = S "Modular Combined"
  showChs (Modular Separated)= S "Modular Separated"

-- | Options for input modules.
data InputModule = Combined -- ^ Input-related functions combined in one module.
                 | Separated -- ^ Input-related functions each in own module.

-- | Determines whether there is a 'Combined' input module or many 'Separated' input 
-- modules, based on a 'Choices' structure. An 'Unmodular' design implicitly means 
-- that input modules are 'Combined'.
inputModule :: Choices -> InputModule
inputModule c = inputModule' $ modularity $ architecture c
  where inputModule' Unmodular = Combined
        inputModule' (Modular im) = im

-- | Program implementation options.
data ImplementationType = Library -- ^ Generated code does not include Controller.
                        | Program -- ^ Generated code includes Controller.

-- | Renders options for program implementation.
instance RenderChoices ImplementationType where
  showChs Library = S "Library"
  showChs Program = S "Program"

-- | Data of a program - how information should be encoded.
data DataInfo = DataInfo {
  -- | Structure of inputs (bundled or not).
  inputStructure :: Structure,
  -- | Structure of constants (inlined or bundled or not, or stored with inputs).
  constStructure :: ConstantStructure,
  -- | Representation of constants (as variables or as constants).
  constRepr :: ConstantRepr
}
-- | Constructor to create a DataInfo
makeData :: Structure -> ConstantStructure -> ConstantRepr -> DataInfo
makeData = DataInfo

-- | Variable structure options.
data Structure = Unbundled -- ^ Individual variables
               | Bundled -- ^ Variables bundled in a class

-- | Renders the structure of variables in a program.
instance RenderChoices Structure where
  showChs Unbundled = S "Unbundled"
  showChs Bundled = S "Bundled"

-- | Constants options.
data ConstantStructure = Inline -- ^ Inline values for constants.
                       | WithInputs -- ^ Store constants with inputs.
                       | Store Structure -- ^ Store constants separately from 
                                         -- inputs, whether bundled or unbundled.

-- | Renders the structure of constants in a program.
instance RenderChoices ConstantStructure where
  showChs Inline = S "Inline"
  showChs WithInputs = S "WithInputs"
  showChs (Store Unbundled) = S "Store Unbundled"
  showChs (Store Bundled) = S "Store Bundled"

-- | Options for representing constants in a program.
data ConstantRepr = Var -- ^ Constants represented as regular variables.
                  | Const -- ^ Use target language's mechanism for defining constants.

-- | Renders the representation of constants in a program.
instance RenderChoices ConstantRepr where
  showChs Var = S "Var"
  showChs Const = S "Const"

-- | Maps for Concepts and Space 
data Maps = Maps {
  -- | Map of 'UID's for Drasil concepts to code concepts.
  -- Matching a 'UID' to a code concept means the code concept should be used
  -- instead of the chunk associated with the 'UID'.
  conceptMatch :: ConceptMatchMap,
  -- | Map of 'Space's to 'CodeType's
  -- Matching a 'Space' to a 'CodeType' means values of the 'Space' should have that
  -- 'CodeType' in the generated code.
  spaceMatch :: SpaceMatch
}
-- | Constructor to create a Maps
makeMaps :: ConceptMatchMap -> SpaceMatch -> Maps
makeMaps = Maps

-- | Specifies matches between chunks and 'CodeConcept's, meaning the target 
-- language's pre-existing definition of the concept should be used instead of 
-- defining a new variable for the concept in the generated code. 
-- ['CodeConcept'] is preferentially-ordered, generator concretizes a 
-- 'ConceptMatchMap' to a 'MatchedConceptMap' by checking user's other choices.
type ConceptMatchMap = Map UID [CodeConcept]
-- | Concrete version of ConceptMatchMap dependent on user choices.
type MatchedConceptMap = Map UID CodeConcept

-- Currently we only support one code concept, more will be added later
-- | Code concepts. For now, just pi.
data CodeConcept = Pi deriving Eq

-- | Renders 'CodeConcept's.
instance RenderChoices CodeConcept where
  showChs Pi = S "Pi"

-- | Builds a 'ConceptMatchMap' from an association list of chunks and 'CodeConcepts'.
matchConcepts :: (HasUID c) => [(c, [CodeConcept])] -> ConceptMatchMap
matchConcepts = fromList . map (\(cnc,cdc) -> (cnc ^. uid, cdc))

-- | Specifies which 'CodeType' should be used to represent each mathematical 
-- 'Space'. ['CodeType'] is preferentially-ordered, first 'CodeType' that does not 
-- conflict with other choices will be selected.
type SpaceMatch = Space -> [CodeType]

-- | Updates a 'SpaceMatch' by matching the given 'Space' with the given ['CodeType'].
matchSpace :: Space -> [CodeType] -> SpaceMatch -> SpaceMatch
matchSpace _ [] _ = error "Must match each Space to at least one CodeType"
matchSpace s ts sm = \sp -> if sp == s then ts else sm sp

-- | Builds a 'SpaceMatch' from an association list of 'Spaces' and 'CodeTypes'.
matchSpaces :: [(Space, [CodeType])] -> SpaceMatch
matchSpaces spMtchs = matchSpaces' spMtchs spaceToCodeType
  where matchSpaces' ((s,ct):sms) sm = matchSpaces' sms $ matchSpace s ct sm
        matchSpaces' [] sm = sm

-- Optional Features can be added to the program or left it out
data OptionalFeatures = OptFeats{
  docConfig :: DocConfig,
  logConfig :: LogConfig,
  -- | Turns generation of different auxiliary (non-source-code) files on or off.
  auxFiles :: [AuxFile]
}
-- | Constructor to create a OptionalFeatures
makeOptFeats :: DocConfig -> LogConfig -> [AuxFile] -> OptionalFeatures
makeOptFeats = OptFeats

-- | Configuration for Doxygen documentation 
data DocConfig = DocConfig {
  -- | Turns Doxygen comments for different code structures on or off.
  comments :: [Comments],
  -- | Standard output from running Doxygen: verbose or quiet?
  doxVerbosity :: Verbosity,
  -- | Turns date field on or off in the generated module-level Doxygen comments.
  dates :: Visibility
}
-- | Constructor to create a DocConfig
makeDocConfig :: [Comments] -> Verbosity -> Visibility -> DocConfig
makeDocConfig = DocConfig

-- | Comment implementation options.
data Comments = CommentFunc -- ^ Function/method-level comments.
              | CommentClass -- ^ Class-level comments.
              | CommentMod -- ^ File/Module-level comments.
              deriving Eq

-- | Renders options for implementation of comments.
instance RenderChoices Comments where
  showChs CommentFunc = S "CommentFunc"
  showChs CommentClass = S "CommentClass"
  showChs CommentMod = S "CommentMod"

-- | Doxygen file verbosity options.
data Verbosity = Verbose | Quiet

-- | Renders options for doxygen verbosity.
instance RenderChoices Verbosity where
  showChs Verbose = S "Verbose"
  showChs Quiet = S "Quiet"

-- | Doxygen date-field visibility options.
data Visibility = Show
                | Hide

-- | Renders options for doxygen date-field visibility.
instance RenderChoices Visibility where
  showChs Show = S "Show"
  showChs Hide = S "Hide"

-- | Log Configuration
data LogConfig = LogConfig {
  -- | Turns different forms of logging on or off.
  logging :: [Logging],
  -- | Name of log file.
  logFile :: FilePath
}
-- | Constructor to create a LogConfig
makeLogConfig :: [Logging] -> FilePath -> LogConfig
makeLogConfig = LogConfig

-- | Logging options for function calls and variable assignments.
-- Eq instances required for Logging and Comments because generator needs to 
-- check membership of these elements in lists
data Logging = LogFunc -- ^ Log messages generated for function calls.
             | LogVar -- ^ Log messages generated for variable assignments.
             deriving Eq

-- | Renders options for program logging.
instance RenderChoices Logging where
  showChs LogFunc = S "LogFunc"
  showChs LogVar = S "LogVar"

-- | Currently we only support two kind of auxiliary files: sample input file, readme.
-- To generate a sample input file compatible with the generated program,
-- 'FilePath' is the path to the user-provided file containing a sample set of input data.
data AuxFile = SampleInput FilePath
             | ReadME
             deriving Eq

-- | Renders options for auxiliary file generation.
instance RenderChoices AuxFile where
  showChs (SampleInput fp) = S "SampleInput" +:+ S fp
  showChs ReadME = S "ReadME"

-- | Gets the file path to a sample input data set from a 'Choices' structure, if 
-- the user chose to generate a sample input file.
getSampleData :: Choices -> Maybe FilePath
getSampleData chs = getSampleData' (auxFiles $ optFeats chs)
  where getSampleData' [] = Nothing
        getSampleData' (SampleInput fp:_) = Just fp
        getSampleData' (_:xs) = getSampleData' xs

-- | Predicate that returns true if the list of 'AuxFile's includes a 'SampleInput'.
hasSampleInput :: [AuxFile] -> Bool
hasSampleInput [] = False
hasSampleInput (SampleInput _:_) = True
hasSampleInput (_:xs) = hasSampleInput xs

-- | SRS Constraints
data Constraints = Constraints{
  onSfwrConstraint :: ConstraintBehaviour,
  onPhysConstraint :: ConstraintBehaviour
}
-- | Constructor to create a Constraints
makeConstraints :: ConstraintBehaviour -> ConstraintBehaviour -> Constraints
makeConstraints = Constraints

-- | Constraint behaviour options within program.
data ConstraintBehaviour = Warning -- ^ Print warning when constraint violated.
                         | Exception -- ^ Throw exception when constraint violated.

-- | Renders options for program implementation.
instance RenderChoices ConstraintBehaviour where
  showChs Warning = S "Warning"
  showChs Exception = S "Exception"

-- | External Library Options
newtype ExtLib = Math ODE

-- | All Information needed to solve an ODE 
data ODE = ODE{
  -- FIXME: ODEInfos should be automatically built from Instance models when 
  -- needed, but we can't do that yet so I'm passing it through Choices instead.
  -- This choice should really just be for an ODEMethod
  -- | ODE information.
  odeInfo :: [ODEInfo],
  -- | Preferentially-ordered list ODE libraries to try.
  odeLib :: [ODELibPckg]
}
-- | Constructor to create an ODE
makeODE :: [ODEInfo] -> [ODELibPckg] -> ODE
makeODE = ODE

-- | Default choices to be used as the base from which design specifications 
-- can be built.
defaultChoices :: Choices
defaultChoices = Choices {
  lang = [Python],
  architecture = makeArchit (Modular Combined) Program,
  dataInfo = makeData Bundled Inline Const,
  maps = makeMaps 
    (matchConcepts ([] :: [(SimpleQDef, [CodeConcept])])) 
    spaceToCodeType,
  optFeats = makeOptFeats 
    (makeDocConfig [] Verbose Hide) 
    (makeLogConfig [] "log.txt") 
    [ReadME],
  srsConstraints = makeConstraints Exception Warning,
  extLibs = []
}

-- | Renders 'Choices' as 'Sentence's.
choicesSent :: Choices -> [Sentence]
choicesSent chs = map chsFieldSent [
    (S "Languages",                     foldlSent_ $ map (S . show) $ lang chs), 
    (S "Modularity",                    showChs $ modularity $ architecture chs), 
    (S "Input Structure",               showChs $ inputStructure $ dataInfo chs), 
    (S "Constant Structure",            showChs $ constStructure $ dataInfo chs), 
    (S "Constant Representation",       showChs $ constRepr $ dataInfo chs), 
    (S "Implementation Type",           showChs $ impType $ architecture chs), 
    (S "Software Constraint Behaviour", showChs $ onSfwrConstraint $ srsConstraints chs), 
    (S "Physical Constraint Behaviour", showChs $ onPhysConstraint $ srsConstraints chs), 
    (S "Comments",                      showChsList $ comments $ docConfig $ optFeats chs), 
    (S "Dox Verbosity",                 showChs $ doxVerbosity $ docConfig $ optFeats chs), 
    (S "Dates",                         showChs $ dates $ docConfig $ optFeats chs), 
    (S "Log File Name",                 S $ logFile $ logConfig $ optFeats chs), 
    (S "Logging",                       showChsList $ logging $ logConfig $ optFeats chs), 
    (S "Auxiliary Files",               showChsList $ auxFiles $ optFeats chs)
  ]

-- | Helper to combine pairs of 'Sentence's for rendering 'Choices'.
chsFieldSent :: (Sentence, Sentence) -> Sentence
chsFieldSent (rec, chc) = rec +:+ S "selected as" +:+. chc