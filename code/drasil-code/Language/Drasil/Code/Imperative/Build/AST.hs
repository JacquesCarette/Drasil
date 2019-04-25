module Language.Drasil.Code.Imperative.Build.AST where

data RunType = Standalone
             | Interpreter String

data RunName = RMain
              | RPackName
              | RPack RunName
              | RWithExt RunName Ext
              | RLit String
              | RConcat RunName RunName

data Ext = CodeExt
         | OtherExt String

data Runnable = Runnable RunName NameOpts RunType

data NameOpts = NameOpts {
  packSep :: String,
  includeExt :: Bool
}

nameOpts :: NameOpts
nameOpts = NameOpts {
  packSep = "/",
  includeExt = True
}

type InterpreterCommand = String

nativeBinary :: Runnable
nativeBinary = Runnable (RConcat RPackName $ RLit "$(TARGET_EXTENSION)") nameOpts Standalone

interp :: RunName -> NameOpts -> InterpreterCommand -> Runnable
interp r n i = Runnable r n $ Interpreter i

interpMM :: InterpreterCommand -> Runnable
interpMM = Runnable (RWithExt RMain CodeExt) nameOpts . Interpreter

mainModule :: RunName
mainModule = RMain

inCodePackage :: RunName -> RunName
inCodePackage = RPack

withExt :: RunName -> String -> RunName
withExt r = RWithExt r . OtherExt