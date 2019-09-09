module GOOL.Drasil.Build.AST where
import Build.Drasil (makeS, MakeString, mkImplicitVar, mkWindowsVar)

type CommandFragment = MakeString

data BuildName = BMain
               | BPackName
               | BPack BuildName
               | BWithExt BuildName Ext

data Ext = CodeExt
         | OtherExt MakeString


data BuildDependencies = BcSource
                       | BcSingle BuildName

data BuildConfig = BuildConfig ([CommandFragment] -> CommandFragment -> BuildCommand) BuildDependencies

data RunType = Standalone
             | Interpreter CommandFragment

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

type BuildCommand = [CommandFragment]
type InterpreterCommand = String

asFragment :: String -> CommandFragment
asFragment = makeS

osClassDefault :: String -> String -> String -> CommandFragment
osClassDefault = mkWindowsVar

buildAll :: ([CommandFragment] -> CommandFragment -> BuildCommand) -> Maybe BuildConfig
buildAll = Just . flip BuildConfig BcSource

buildSingle :: ([CommandFragment] -> CommandFragment -> BuildCommand) -> BuildName -> Maybe BuildConfig
buildSingle f = Just . BuildConfig f . BcSingle

nativeBinary :: Runnable
nativeBinary = Runnable (BWithExt BPackName $ OtherExt $
  osClassDefault "TARGET_EXTENSION" ".exe" "") nameOpts Standalone

interp :: BuildName -> NameOpts -> InterpreterCommand -> Runnable
interp b n = Runnable b n . Interpreter . makeS

interpMM :: InterpreterCommand -> Runnable
interpMM = Runnable mainModuleFile nameOpts . Interpreter . makeS

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
