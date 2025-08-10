{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Language.Drasil.WellTyped where

import qualified Data.Map.Strict as M
import Drasil.Database.UID (UID)
import Data.List (intercalate)

-- TODO: Rather than using a flat String, it would be better to a
--       ``breadcrumb''-style error data type that can provide a traceable path
--       to issues in an expression. The breadcrumbs could then be formatted
--       into a flat String, such as it is now, or into alternative AST views
--       that show exactly where typing went awry.
type TypeError = String

-- | We can only type check 'UID's within a type context relating 'UID's to
--   types since they don't carry any type information.
type TypingContext t = M.Map UID t

-- | Look for a known type of a specific 'UID'.
inferFromContext :: TypingContext t -> UID -> Either TypeError t
inferFromContext cxt u =
  case M.lookup u cxt of
    Just t  -> pure t
    Nothing -> Left $ "`" ++ show u ++ "` lacks type binding in context"

-- | Build a bidirectional type checker for your expression language, e, with
--   respect to a specific type universe, t.
class (Eq t, Show t) => Typed e t where

  -- | Given a typing context and an expression, infer a unique type or explain
  --   what went awry.
  infer :: TypingContext t -> e -> Either TypeError t

  -- | Given a typing context, an expression, and an expected type, check if the
  --   expression can satisfy the expectation.
  check :: TypingContext t -> e -> t -> Either TypeError t

-- | For all containers, c, which contain typed expressions, e, against a
--   specific type universe, t, expose all expressions and relations that need
--   to be type-checked.
class Typed e t => RequiresChecking c e t where
  -- | All things that need type checking.
  requiredChecks :: c -> [(e, t)]

assertEq :: (Show t, Eq t) => t -> t -> (String -> String -> TypeError) -> Either TypeError ()
assertEq lt rt te
  | lt == rt  = pure ()
  | otherwise = Left $ te (show lt) (show rt)

infix 4 ~==
(~==) :: (Show t, Eq t) => t -> t -> (String -> String -> TypeError) -> Either TypeError ()
(~==) = assertEq

-- | ``Check'' an expressions type based by an inference.
typeCheckByInfer :: Typed e t => TypingContext t -> e -> t -> Either TypeError t
typeCheckByInfer cxt e t = do
  et <- infer cxt e
  et ~== t 
    $ \lt rt -> "Inferred type `" ++ lt ++ "` does not match expected type `" ++ rt ++ "`"
  pure et

{- FIXME: temporary hacks below (they're aware of the "printing" code!), pending
          replacement when TypeError is upgraded -}

assertAllEq :: Typed e t => TypingContext t -> [e] -> t -> TypeError -> Either TypeError ()
assertAllEq cxt es expect s
  | allTsAreSp = pure ()
  | otherwise  = Left $ temporaryIndent "  " (s ++ "\nReceived:\n" ++ dumpAllTs)
  where
    allTs = map (infer cxt) es
    allTsAreSp = all (\case
      Right  t -> t == expect
      Left _   -> False) allTs
    dumpAllTs = intercalate "\n" $ map (("- " ++) . either ("ERROR: " ++) show) allTs

-- | A temporary, hacky, indentation function. It should be removed when we
-- switch to using something else for error messages, which can be later
-- formatted nicely.
temporaryIndent :: String -> String -> String
temporaryIndent r ('\n' : s) = '\n' : (r ++ temporaryIndent r s)
temporaryIndent r (c    : s) = c : temporaryIndent r s
temporaryIndent _ []         = []
