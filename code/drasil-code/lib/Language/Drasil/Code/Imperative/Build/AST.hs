module Language.Drasil.Code.Imperative.Build.AST where
import Build.Drasil (makeS, MakeString, mkImplicitVar, mkWindowsVar, mkOSVar,
  Command, mkCheckedCommand, Dependencies)

-- | Used to build commands. Type synonym of a 'MakeString' for clarity.
type CommandFragment = MakeString

-- | Type for holding the build name.
data BuildName = BMain
               | BPackName
               | BPack BuildName
               | BWithExt BuildName Ext

-- | File extentions.
data Ext = CodeExt
         | OtherExt MakeString

-- | Builds may only require themselves or have a dependency.
data BuildDependencies = BcSource
                       | BcSingle BuildName

-- | Build configuration. In the function parameter, the first parameter is the list of inputs,
-- 2nd parameter is the output file, 3rd parameter is additional name if needed.
-- The two 'Maybe' 'BuildName's are the output file and the additional name.
-- Also holds the build dependencies.
data BuildConfig = BuildConfig
  ([CommandFragment] -> CommandFragment -> CommandFragment -> [BuildCommand])
  (Maybe BuildName) (Maybe BuildName) BuildDependencies

-- | Run commands as they are or through an interpreter.
data RunType = Standalone
             | Interpreter [CommandFragment]

-- | Contains all the information needed to run a command.
data Runnable = Runnable BuildName NameOpts RunType

-- | Configures a document based on dependencies and commands.
data DocConfig = DocConfig Dependencies [Command]

-- | Naming options. Includes a package separator and an option for including extensions.
data NameOpts = NameOpts {
  packSep :: String,
  includeExt :: Bool
}

-- | Default name options. Packages separately by "/" and includes extension.
nameOpts :: NameOpts
nameOpts = NameOpts {
  packSep = "/",
  includeExt = True
}

-- | Build commands. Made up of 'CommandFragment's.
type BuildCommand = [CommandFragment]
-- | Interpreter commands are made up of a 'String'.
type InterpreterCommand = String
-- | Interpreter options are made up of a 'String'.
type InterpreterOption = String

-- | Translates a 'String' into a command fragment.
asFragment :: String -> CommandFragment
asFragment = makeS

-- | OS default variable (Windows). Calls 'mkWindowsVar'.
osClassDefault :: String -> String -> String -> CommandFragment
osClassDefault = mkWindowsVar

-- | Constructor for a build configuration. No additional name included.
buildAll :: ([CommandFragment] -> CommandFragment -> [BuildCommand]) ->
  BuildName -> Maybe BuildConfig
buildAll f n = Just $ BuildConfig (\i o _ -> f i o) (Just n) Nothing BcSource

-- | Constructor for a build configuration with an additional name included.
buildAllAdditionalName :: ([CommandFragment] -> CommandFragment ->
  CommandFragment -> [BuildCommand]) -> BuildName -> BuildName ->
  Maybe BuildConfig
buildAllAdditionalName f n a = Just $ BuildConfig f (Just n) (Just a) BcSource

-- | Constructor for a build configuration.
-- No additional name included, but takes in a single dependency.
buildSingle :: ([CommandFragment] -> CommandFragment -> [BuildCommand]) ->
  BuildName -> BuildName -> Maybe BuildConfig
buildSingle f n = Just . BuildConfig (\i o _ -> f i o) (Just n) Nothing .
  BcSingle

-- | Default runnable information.
nativeBinary :: Maybe Runnable
nativeBinary = Just $ Runnable executable nameOpts Standalone

-- | Default target extension is ".exe".
executable :: BuildName
executable = BWithExt BPackName $ OtherExt $
  osClassDefault "TARGET_EXTENSION" ".exe" ""

-- | Default library has the extentions ".dll, .dylib, .so".
sharedLibrary :: BuildName
sharedLibrary = BWithExt BPackName $ OtherExt $
  mkOSVar "LIB_EXTENSION" ".dll" ".dylib" ".so"

-- | Constructor for a runnable command that goes through an interpreter.
interp :: BuildName -> NameOpts -> InterpreterCommand -> [InterpreterOption]
  -> Maybe Runnable
interp b n c = Just . Runnable b n . Interpreter . map makeS . (c:)

-- | Constructs a runnable command that goes through an interpreter (for main module file).
interpMM :: InterpreterCommand -> Maybe Runnable
interpMM = Just . Runnable mainModuleFile nameOpts . Interpreter . (:[]) . makeS

-- | Main module.
mainModule :: BuildName
mainModule = BMain

-- | Main module with a default code extension.
mainModuleFile :: BuildName
mainModuleFile = BWithExt BMain CodeExt

-- | Module for an in-code package.
inCodePackage :: BuildName -> BuildName
inCodePackage = BPack

-- | Adds an extension to a 'BuildName'.
withExt :: BuildName -> String -> BuildName
withExt b = BWithExt b . OtherExt . makeS

-- | C compiler command fragment.
cCompiler :: CommandFragment
cCompiler = mkImplicitVar "CC"

-- | C++ compiler command fragment.
cppCompiler :: CommandFragment
cppCompiler = mkImplicitVar "CXX"

-- | Helper for configuring doxygen documentation.
doxygenDocConfig :: FilePath -> DocConfig
doxygenDocConfig fp = DocConfig [makeS fp]
  [mkCheckedCommand $ makeS $ "doxygen " ++ fp]
