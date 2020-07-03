module Language.Drasil.Code.Imperative.Build.AST where
import Build.Drasil (makeS, MakeString, mkImplicitVar, mkWindowsVar, mkOSVar,
  Command, mkCheckedCommand, Dependencies)

type CommandFragment = MakeString

data BuildName = BMain
               | BPackName
               | BPack BuildName
               | BWithExt BuildName Ext

data Ext = CodeExt
         | OtherExt MakeString

data BuildDependencies = BcSource
                       | BcSingle BuildName

-- In the function parameter, first parameter is the list of inputs, 2nd parameter is the output file, 3rd parameter is additional name if needed. 
-- The two Maybe BuildNames are the output file and the additional name.
data BuildConfig = BuildConfig 
  ([CommandFragment] -> CommandFragment -> CommandFragment -> [BuildCommand]) 
  (Maybe BuildName) (Maybe BuildName) BuildDependencies

data RunType = Standalone
             | Interpreter [CommandFragment]

data Runnable = Runnable BuildName NameOpts RunType

data DocConfig = DocConfig Dependencies [Command]

data NameOpts = NameOpts {
  packSep :: String,
  includeExt :: Bool
}

nameOpts :: NameOpts
nameOpts = NameOpts {
  packSep = "/",
  includeExt = True
}

type BuildCommand = [CommandFragment]
type InterpreterCommand = String
type InterpreterOption = String

asFragment :: String -> CommandFragment
asFragment = makeS

osClassDefault :: String -> String -> String -> CommandFragment
osClassDefault = mkWindowsVar

buildAll :: ([CommandFragment] -> CommandFragment -> [BuildCommand]) -> 
  BuildName -> Maybe BuildConfig
buildAll f n = Just $ BuildConfig (\i o _ -> f i o) (Just n) Nothing BcSource

buildAllAdditionalName :: ([CommandFragment] -> CommandFragment -> 
  CommandFragment -> [BuildCommand]) -> BuildName -> BuildName -> 
  Maybe BuildConfig
buildAllAdditionalName f n a = Just $ BuildConfig f (Just n) (Just a) BcSource

buildSingle :: ([CommandFragment] -> CommandFragment -> [BuildCommand]) -> 
  BuildName -> BuildName -> Maybe BuildConfig
buildSingle f n = Just . BuildConfig (\i o _ -> f i o) (Just n) Nothing . 
  BcSingle

nativeBinary :: Maybe Runnable
nativeBinary = Just $ Runnable executable nameOpts Standalone

executable :: BuildName 
executable = BWithExt BPackName $ OtherExt $ 
  osClassDefault "TARGET_EXTENSION" ".exe" ""

sharedLibrary :: BuildName
sharedLibrary = BWithExt BPackName $ OtherExt $
  mkOSVar "LIB_EXTENSION" ".dll" ".dylib" ".so"

interp :: BuildName -> NameOpts -> InterpreterCommand -> [InterpreterOption]
  -> Maybe Runnable
interp b n c = Just . Runnable b n . Interpreter . map makeS . (c:)

interpMM :: InterpreterCommand -> Maybe Runnable
interpMM = Just . Runnable mainModuleFile nameOpts . Interpreter . (:[]) . makeS

mainModule :: BuildName
mainModule = BMain

mainModuleFile :: BuildName
mainModuleFile = BWithExt BMain CodeExt

inCodePackage :: BuildName -> BuildName
inCodePackage = BPack

withExt :: BuildName -> String -> BuildName
withExt b = BWithExt b . OtherExt . makeS

cCompiler :: CommandFragment
cCompiler = mkImplicitVar "CC"

cppCompiler :: CommandFragment
cppCompiler = mkImplicitVar "CXX"

doxygenDocConfig :: FilePath -> DocConfig
doxygenDocConfig fp = DocConfig [makeS fp] 
  [mkCheckedCommand $ makeS $ "doxygen " ++ fp]