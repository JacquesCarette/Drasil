{-# LANGUAGE TemplateHaskellQuotes #-}

module Drasil.Build.Artifacts.FilePath
  ( PathComponent,
    rpc,
    (</>),
    createDirectory,
    doesPathExist,
    writeFile,
    writeFile',
  )
where

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.Directory qualified as Dir (createDirectory, doesPathExist)
import System.FilePath qualified as FP (pathSeparator, (</>))
import System.IO qualified as IO (Handle, IOMode (WriteMode), withFile)
import Prelude hiding (writeFile)
import Prelude qualified (writeFile)

-- | Represents a valid component of a path (e.g., a file or directory /name/).
newtype PathComponent = PC {unPC :: String}
  deriving (Eq, Ord, Show)

(</>) :: PathComponent -> PathComponent -> PathComponent
(PC a) </> (PC b) = PC $ a FP.</> b

doesPathExist :: PathComponent -> IO Bool
doesPathExist = Dir.doesPathExist . unPC

createDirectory :: PathComponent -> IO ()
createDirectory = Dir.createDirectory . unPC

writeFile :: PathComponent -> String -> IO ()
writeFile (PC pc) = Prelude.writeFile pc

writeFile' :: PathComponent -> (IO.Handle -> IO r) -> IO r
writeFile' (PC pc) = IO.withFile pc IO.WriteMode

rpc :: QuasiQuoter
rpc =
  QuasiQuoter
    { quoteExp = parse,
      quotePat = unpermitted,
      quoteType = unpermitted,
      quoteDec = unpermitted
    }
  where
    unpermitted _ = fail "quasiquoting paths only permitted as Haskell expressions"

-- | Internal: Check if a path component (i.e., text before/between/after path
-- separators) is a valid component. Here, validity being defined by not being
-- any of: ., .., ~, or the system-local path separator.
parse :: String -> Q Exp
parse s
  | s `elem` [".", "..", "~"] = fail $ "invalid path component: " ++ show s ++ "."
  | FP.pathSeparator `elem` s = fail $ "cannot create path component with " ++ show FP.pathSeparator ++ " in the name."
  | otherwise = [|PC s|]
