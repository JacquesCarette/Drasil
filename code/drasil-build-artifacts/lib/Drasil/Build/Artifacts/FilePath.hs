{-# LANGUAGE TemplateHaskell #-}

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

import Language.Haskell.TH (Exp, Q, listE, mkName, stringE, varE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import System.FilePath (pathSeparator)
import System.OsPath (OsPath, encodeUtf)
import System.OsPath qualified as FP ((</>))
import Prelude hiding (writeFile)

-- | Represents a single valid segment of a path (e.g., a file or directory
-- /name/, usually the terminal basename of a path). Does not allow path
-- segments to be any of: `..`, `.`, nor `~`. Assumes case-sensitive equality.
newtype PathSegment = PS {unPS :: OsPath}
  deriving (Eq, Ord)

-- | Retrieve 'OsPath' representation of a 'PathSegment'.
toPath :: PathSegment -> OsPath
toPath = unPS
-- NOTE: 'toPath' is exported (& exists) so that 'PathSegment's internal
-- 'OsPath' can be read, but a 'PathSegment' can never be updated via normal
-- accessor update.
{-# INLINE toPath #-}

-- | Append a terminal 'PathSegment' onto a 'OsPath' to form a new 'OsPath'.
(</>) :: OsPath -> PathSegment -> OsPath
a </> (PS b) = a FP.</> b
{-# INLINE (</>) #-}

-- | 'QuasiQuoter' for building 'PathSegment's.
--
-- Syntax:
--   ps ::= comp+
--   comp ::= '{' hsVar '}'
--         |  <any string excl. '.', '..', '~', not
--             including the system-local path sep.>
--   hsVar ::= <any string>
ps :: QuasiQuoter
ps =
  QuasiQuoter
    { quoteExp = qPathSeg,
      quotePat = unpermitted,
      quoteType = unpermitted,
      quoteDec = unpermitted
    }
  where
    unpermitted _ = fail "quasiquoting paths only permitted for Haskell expressions"

-- | Internal: Quote a 'String' into a 'PathSegment'.
qPathSeg :: String -> Q Exp
qPathSeg [] = fail "empty path"
qPathSeg s = do
  comps <- pathSegComps s
  case comps of
    [Str s'] -> do
      p <- either fail pure (validatePathSegStr s')
      [|PS p|]
    _ ->
      [|mkPathSegOrErr $ concat $(listE $ map pathSegCompToQExp comps)|]

-- | Internal: Constructor for dynamic path splices.
mkPathSegOrErr :: String -> PathSegment
mkPathSegOrErr = either error PS . validatePathSegStr

-- | Internal: Check if a path segment (i.e., text before/between/after path
-- separators) is a valid segment. Here, validity being defined by being
-- non-empty, not containing the system-local path separator, nor being any of:
-- ., .., or ~.
validatePathSegStr :: String -> Either String OsPath
validatePathSegStr [] = Left "empty path"
validatePathSegStr s
  | s `elem` [".", "..", "~"] = Left $ "invalid path segment: " ++ show s ++ "."
  | pathSeparator `elem` s = Left $ "cannot create path segment with " ++ show pathSeparator ++ " in the name."
  | otherwise = either (Left . ("invalid os path: " ++) . show) pure (encodeUtf s)

-- | Internal: A Haskell variable is just a string. We will make no effort to
-- audit them because Haskell supports unicode variable names.
type HaskellVar = String

-- | Internal: A path segment component is either a static 'String' or an
-- arbitrary Haskell expression.
data PathSegmentComponent
  = Str String
  | HsVar HaskellVar
  deriving (Eq, Show)

-- | Internal: Lift a 'PathSegmentComponent' into a Template Haskell expression.
pathSegCompToQExp :: PathSegmentComponent -> Q Exp
pathSegCompToQExp (Str s) = stringE s
pathSegCompToQExp (HsVar v) = varE (mkName v)

-- | Internal: Parse 'PathSegmentComponent's from a 'String' using sub-'String's
-- of the form `{x}` to indicate Haskell-level variables.
pathSegComps :: (MonadFail m) => String -> m [PathSegmentComponent]
pathSegComps = go []
  where
    go acc [] = finAcc acc
    go acc ('\\' : '{' : xs) = go ('{' : acc) xs
    go acc ('{' : xs) =
      case break (== '}') xs of
        (_, []) -> fail "missing closing } in Haskell variable reference"
        (var, _ : rest) -> do
          prefix <- finAcc acc
          next <- go [] rest
          pure $ prefix ++ HsVar var : next
    go acc (x : xs) = go (x : acc) xs

    finAcc [] = pure []
    finAcc as = pure [Str (reverse as)]
