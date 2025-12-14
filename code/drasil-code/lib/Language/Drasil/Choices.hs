-- | Defines the design language for SCS.
module Language.Drasil.Choices (
  Choices(..), Architecture (..), makeArchit, DataInfo(..), makeData, Maps(..),
  makeMaps, spaceToCodeType, Constraints(..), makeConstraints, ODE(..), makeODE,
  DocConfig(..), makeDocConfig, LogConfig(..), makeLogConfig, OptionalFeatures(..),
  makeOptFeats, ExtLib(..), Modularity(..), Structure(..),
  ConstantStructure(..), ConstantRepr(..), ConceptMatchMap, MatchedConceptMap,
  CodeConcept(..), matchConcepts, chooseConcept, conceptToGOOL,
  SpaceMatch, matchSpaces, ImplementationType(..),
  ConstraintBehaviour(..), Comments(..), Verbosity(..), Visibility(..),
  Logging(..), AuxFile(..), getSampleData, hasSampleInput, defaultChoices,
  choicesSent, showChs, InternalConcept(..)
) where

import Prelude hiding (pi)

import Control.Lens ((^.))
import Control.Monad.State (State, modify)
import Data.List.NonEmpty (toList)
import Data.Map (Map)
import qualified Data.Map as Map

import Drasil.Database (UID, HasUID (..))
import Drasil.GOOL (SValue, SharedProg, MathConstant(..))
import qualified Drasil.GOOL as C (CodeType(..))
import Language.Drasil (Sentence(S), (+:+.), (+:+), foldlSent_, Space, SimpleQDef)
import qualified Language.Drasil as S (Space(..))
import Language.Drasil.Code.Lang (Lang(..))
import Language.Drasil.Data.ODEInfo (ODEInfo)
import Language.Drasil.Data.ODELibPckg (ODELibPckg)
import Language.Drasil.Mod (Name)

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
  extLibs :: [ExtLib],
  -- | Function to get modifiable function names
  icNames :: InternalConcept -> Name,
  -- | Number of folders to go up in order to obtain the image
  folderVal :: Int
}

-- | Renders program choices as a 'Sentence'.
class RenderChoices a where
    showChs :: a -> Sentence
    showChsList :: [a] -> Sentence
    showChsList [] = S "None"
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
data Modularity = Modular   -- ^ Different modules. For controller,
                              -- input, calculations, output.
                | Unmodular -- ^ All generated code is in one module/file.

-- | Renders the modularity of a program.
instance RenderChoices Modularity where
  showChs Unmodular = S "Unmodular"
  showChs Modular = S "Modular"

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
matchConcepts = Map.fromList . map (\(cnc,cdc) -> (cnc ^. uid, cdc))

-- | Concretizes the ConceptMatchMap in Choices to a 'MatchedConceptMap'.
-- Currently we don't have any Choices that would prevent a 'CodeConcept' from
-- being mapped, so we just take the head of the list of 'CodeConcept's
-- The ConceptMatchMap from choices is passed to chooseConcept' internally, this way
-- any 'CodeConcept' list can be matched to its appropiate 'UID'.
chooseConcept :: Choices -> State [Sentence] MatchedConceptMap
chooseConcept chs = sequence $ Map.mapWithKey chooseConcept' (conceptMatch $ maps chs)
  where chooseConcept' :: UID -> [CodeConcept] -> State [Sentence] CodeConcept
        chooseConcept' _ [] = error $ "Empty list of CodeConcepts in the " ++
          "ConceptMatchMap"
        chooseConcept' concUid (c:_) = do
            modify (++ [S "Code Concept" +:+ S (show concUid) +:+ S "selected as" +:+. showChs c])
            return c

-- | Translates a 'CodeConcept' into GOOL.
conceptToGOOL :: (SharedProg r) => CodeConcept -> SValue r
conceptToGOOL Pi = pi

-- | Specifies which 'CodeType' should be used to represent each mathematical
-- 'Space'. ['CodeType'] is preferentially-ordered, first 'CodeType' that does not
-- conflict with other choices will be selected.
type SpaceMatch = Space -> [C.CodeType]

-- | Updates a 'SpaceMatch' by matching the given 'Space' with the given ['CodeType'].
matchSpace :: Space -> [C.CodeType] -> SpaceMatch -> SpaceMatch
matchSpace _ [] _ = error "Must match each Space to at least one CodeType"
matchSpace s ts sm = \sp -> if sp == s then ts else sm sp

-- | Builds a 'SpaceMatch' from an association list of 'Spaces' and 'CodeTypes'.
matchSpaces :: [(Space, [C.CodeType])] -> SpaceMatch
matchSpaces spMtchs = matchSpaces' spMtchs spaceToCodeType
  where matchSpaces' ((s,ct):sms) sm = matchSpaces' sms $ matchSpace s ct sm
        matchSpaces' [] sm = sm

-- | Default mapping between 'Space' and 'CodeType'.
spaceToCodeType :: S.Space -> [C.CodeType]
spaceToCodeType S.Integer        = [C.Integer]
spaceToCodeType S.Natural        = [C.Integer]
spaceToCodeType S.Real           = [C.Double, C.Float]
spaceToCodeType S.Rational       = [C.Double, C.Float]
spaceToCodeType S.Boolean        = [C.Boolean]
spaceToCodeType S.Char           = [C.Char]
spaceToCodeType S.String         = [C.String]
spaceToCodeType (S.Vect s)       = map C.List (spaceToCodeType s)
spaceToCodeType (S.Matrix _ _ s) = map (C.List . C.List) (spaceToCodeType s)
spaceToCodeType (S.Set s)        = map C.List (spaceToCodeType s)
spaceToCodeType (S.Array s)      = map C.Array (spaceToCodeType s)
spaceToCodeType (S.Actor s)      = [C.Object s]
spaceToCodeType S.Void           = [C.Void]
spaceToCodeType (S.Function i t) = [C.Func is ts | is <- ins, ts <- trgs]
    where trgs = spaceToCodeType t
          ins  = map spaceToCodeType (toList i)

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
  architecture = makeArchit Modular Program,
  dataInfo = makeData Bundled Inline Const,
  maps = makeMaps
    (matchConcepts ([] :: [(SimpleQDef, [CodeConcept])]))
    spaceToCodeType,
  optFeats = makeOptFeats
    (makeDocConfig [] Verbose Hide)
    (makeLogConfig [] "log.txt")
    [ReadME],
  srsConstraints = makeConstraints Exception Warning,
  extLibs = [],
  icNames = defaultICName,
  folderVal = 4
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

-- | Data type of internal concepts
data InternalConcept =
    InputConstraintsFn
  | InputConstraintsMod
  | WriteOutput
  | DerivedValuesFn
  | DerivedValuesMod
  | GetInput
  | InputParameters
  | InputFormat
  | OutputFormat
  | Calculations
  | Constants
  deriving (Eq, Ord)

-- | Function to get default InternalConcept names
defaultICName :: InternalConcept -> Name
defaultICName InputConstraintsFn  = "input_constraints"
defaultICName InputConstraintsMod = "InputConstraints"
defaultICName WriteOutput         = "write_output"
defaultICName DerivedValuesFn     = "derived_values"
defaultICName DerivedValuesMod    = "DerivedValues"
defaultICName GetInput            = "get_input"
defaultICName InputParameters     = "InputParameters"
defaultICName InputFormat         = "InputFormat"
defaultICName OutputFormat        = "OutputFormat"
defaultICName Calculations        = "Calculations"
defaultICName Constants           = "Constants"
