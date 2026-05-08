{-# LANGUAGE TemplateHaskellQuotes #-}

module Drasil.Build.Artifacts.FilePath
  ( -- * File Path Segments
    PathSegment,

    -- ** Construction
    ps,
    (</>),

    -- ** Unpacking
    toPath,
  )
where

import Control.Exception (SomeException)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.FilePath (pathSeparator)
import System.OsPath (OsPath, encodeUtf)
import System.OsPath qualified as FP ((</>))
import Prelude hiding (writeFile)

-- | Represents a valid segment of a path (e.g., a file or directory /name/, or
-- the name of one parent folder). Does not allow path segments to contain `..`,
-- `.`, `~`, nor the system-local path separator. Case-sensitive equality.
newtype PathSegment = PS {unPS :: OsPath}
  deriving (Eq, Ord)

-- | Retrieve 'OsPath' representation of a 'PathSegment'.
toPath :: PathSegment -> OsPath
toPath = unPS
-- NOTE: 'toPath' is exported (& exists) so that 'PathSegment's internal
-- 'OsPath' can be read, but a 'PathSegment' can never be updated via normal
-- accessor update.
{-# INLINE toPath #-}

-- | Combine a 'OsPath' and a 'PathSegment' into a new 'OsPath'.
(</>) :: OsPath -> PathSegment -> OsPath
a </> (PS b) = a FP.</> b
{-# INLINE (</>) #-}

-- | 'QuasiQuoter' for building 'PathSegment's.
ps :: QuasiQuoter
ps =
  QuasiQuoter
    { quoteExp = qPathSeg,
      quotePat = unpermitted,
      quoteType = unpermitted,
      quoteDec = unpermitted
    }
  where
    unpermitted _ = fail "quasiquoting paths only permitted as Haskell expressions"

-- | Internal: Check if a path segment (i.e., text before/between/after path
-- separators) is a valid segment. Here, validity being defined by not being any
-- of: ., .., ~, or the system-local path separator.
qPathSeg :: String -> Q Exp
qPathSeg s
  | s `elem` [".", "..", "~"] = fail $ "invalid path segment: " ++ show s ++ "."
  | pathSeparator `elem` s = fail $ "cannot create path segment with " ++ show pathSeparator ++ " in the name."
  | otherwise = case encodeUtf s :: Either SomeException OsPath of
      Left err -> fail $ "invalid os path: " ++ show err
      Right osp -> [|PS osp|]
