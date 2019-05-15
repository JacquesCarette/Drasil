module Language.Drasil.Code.Imperative.Build.AST where

data BuildName = BMain
               | BPackName
               | BPack BuildName
               | BWithExt BuildName Ext

data Ext = CodeExt
         | OtherExt String


data BuildDependencies = BcAll
                       | BcSingle BuildName

data BuildConfig = BuildConfig ([String] -> String -> BuildCommand) BuildDependencies

data RunType = Standalone
             | Interpreter String

data Runnable = Runnable BuildName NameOpts RunType

data NameOpts = NameOpts {
  packSep :: String,
  includeExt :: Bool
}

nameOpts :: NameOpts
nameOpts = NameOpts {
  packSep = "/",
  includeExt = True
}

type BuildCommand = [String]
type InterpreterCommand = String

buildAll :: ([String] -> String -> BuildCommand) -> Maybe BuildConfig
buildAll = Just . flip BuildConfig BcAll

buildSingle :: ([String] -> String -> BuildCommand) -> BuildName -> Maybe BuildConfig
buildSingle f = Just . BuildConfig f . BcSingle

nativeBinary :: Runnable
nativeBinary = Runnable (BWithExt BPackName $ OtherExt "$(TARGET_EXTENSION)") nameOpts Standalone

interp :: BuildName -> NameOpts -> InterpreterCommand -> Runnable
interp b n i = Runnable b n $ Interpreter i

interpMM :: InterpreterCommand -> Runnable
interpMM = Runnable mainModuleFile nameOpts . Interpreter

mainModule :: BuildName
mainModule = BMain

mainModuleFile :: BuildName
mainModuleFile = BWithExt BMain CodeExt

inCodePackage :: BuildName -> BuildName
inCodePackage = BPack

withExt :: BuildName -> String -> BuildName
withExt b = BWithExt b . OtherExt

cCompiler :: String
cCompiler = "$(CC)"

cppCompiler :: String
cppCompiler = "$(CXX)"
