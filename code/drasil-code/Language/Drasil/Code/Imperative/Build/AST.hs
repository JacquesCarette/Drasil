module Language.Drasil.Code.Imperative.Build.AST where

data RunType = Standalone
             | Interpreter String

data BuildName = BMain
               | BPackName
               | BPack BuildName
               | BWithExt BuildName Ext

data Ext = CodeExt
         | OtherExt String

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

type InterpreterCommand = String

nativeBinary :: Runnable
nativeBinary = Runnable (BWithExt BPackName $ OtherExt "$(TARGET_EXTENSION)") nameOpts Standalone

interp :: BuildName -> NameOpts -> InterpreterCommand -> Runnable
interp b n i = Runnable b n $ Interpreter i

interpMM :: InterpreterCommand -> Runnable
interpMM = Runnable (BWithExt BMain CodeExt) nameOpts . Interpreter

mainModule :: BuildName
mainModule = BMain

inCodePackage :: BuildName -> BuildName
inCodePackage = BPack

withExt :: BuildName -> String -> BuildName
withExt b = BWithExt b . OtherExt
