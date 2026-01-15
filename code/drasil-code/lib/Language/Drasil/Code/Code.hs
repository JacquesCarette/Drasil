-- | Contains the high-level functionality to create 'Code' and then produce the
-- actual generated code files.
module Language.Drasil.Code.Code (
    Code(..), makeCode,
    createCodeFiles,
    spaceToCodeType
) where

import Text.PrettyPrint.HughesPJ ( Doc, Doc, render )
import Data.List.NonEmpty (toList)

import Drasil.GOOL ( CodeType(..), FileData(..), ModData(modDoc))
import qualified Language.Drasil as S (Space(..))
import Utils.Drasil (createDirIfMissing)

import Language.Drasil.Code.Imperative.GOOL.Data (FileAndContents(fileDoc))
import qualified Language.Drasil.Code.Imperative.GOOL.Data as D (
  filePath)
import System.FilePath.Posix (takeDirectory)
import System.IO (hPutStrLn, hClose, openFile, IOMode(WriteMode))

-- | Represents the generated code as a list of file names and rendered code pairs.
newtype Code = Code { unCode :: [(FilePath, Doc)]}

-- | Makes code from 'FileData' ('FilePath's with module data) and 'FileAndContents'
-- ('FilePath's with auxiliary document information).
makeCode :: [FileData] -> [FileAndContents] -> Code
makeCode files aux = Code $ zip (map filePath files ++ map D.filePath aux)
  (map (modDoc . fileMod) files ++ map fileDoc aux)

-- | Creates the requested 'Code' by producing files.
createCodeFiles :: Code -> IO ()
createCodeFiles (Code cs) = mapM_ createCodeFile cs

-- | Helper that uses pairs of 'Code' to create a file written with the given
-- document at the given 'FilePath'.
createCodeFile :: (FilePath, Doc) -> IO ()
createCodeFile (path, code) = do
  createDirIfMissing True (takeDirectory path)
  h <- openFile path WriteMode
  hPutStrLn h (render code)
  hClose h

-- | Default mapping between 'Space' and 'CodeType'.
spaceToCodeType :: S.Space -> [CodeType]
spaceToCodeType S.Integer        = [Integer]
spaceToCodeType S.Natural        = [Integer]
spaceToCodeType S.Real           = [Double, Float]
spaceToCodeType S.Rational       = [Double, Float]
spaceToCodeType S.Boolean        = [Boolean]
spaceToCodeType S.Char           = [Char]
spaceToCodeType S.String         = [String]
spaceToCodeType (S.Vect s)       = map List (spaceToCodeType s)
spaceToCodeType (S.Matrix _ _ s) = map (List . List) (spaceToCodeType s)
spaceToCodeType (S.Set s)        = map List (spaceToCodeType s)
spaceToCodeType (S.Array s)      = map Array (spaceToCodeType s)
spaceToCodeType (S.Actor s)      = [Object s]
spaceToCodeType S.Void           = [Void]
spaceToCodeType (S.Function i t) = [Func is ts | is <- ins, ts <- trgs]
    where trgs = spaceToCodeType t
          ins  = map spaceToCodeType (toList i)
