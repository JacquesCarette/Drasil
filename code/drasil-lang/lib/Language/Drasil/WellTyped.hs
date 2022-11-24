{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Language.Drasil.WellTyped where

import qualified Data.Map.Strict as M
import Language.Drasil.UID (UID)
import Data.List (intercalate)

-- TODO: TypeError should be a Doc type instead, so that we can have cleaner
-- error messages and formatting. It might be good to have a "breadcrumb"-style
-- error type that shows a path to the problematic section, with an error
-- message.
type TypeError = String

-- | We can only type check 'UID's within a type context relating 'UID's to
-- types since they don't carry any type information.
type TypingContext t = M.Map UID t

inferFromContext :: TypingContext t -> UID -> Either t TypeError
inferFromContext cxt u = maybe
    (Right $ "`" ++ show u ++ "` lacks type binding in context")
    Left
    (M.lookup u cxt)

check :: Typed e t => TypingContext t -> e -> t -> Either t TypeError
check cxt e t = either
    (\infT -> if t == infT
      then Left t
      else Right $ "Inferred type `" ++ show t ++ "` does not match expected type `" ++ show infT ++ "`")
    Right
    (infer cxt e)

class (Eq t, Show t) => Typed e t where
  infer :: TypingContext t -> e -> Either t TypeError

class Typed e t => TypeChecks c e t where
  -- Why is this a list, you ask? So that we can have multiple expressions type
  -- checked at once, which a chunk may or may not expose.
  typeCheckExpr :: c -> [(e, t)]

-- TODO: This should be formatted with Docs rather than hacking together Strings.
allOfType :: Typed e t => TypingContext t -> [e] -> t -> t -> TypeError -> Either t TypeError
allOfType cxt es expect ret s
  | allTsAreSp = Left ret
  | otherwise  = Right $ temporaryIndent "  " (s ++ "\nReceived:\n" ++ dumpAllTs)
  where
    allTs = map (infer cxt) es
    allTsAreSp = all (\case
      Left  t -> t == expect
      Right _ -> False) allTs
    dumpAllTs = intercalate "\n" $ map (("- " ++) . either show ("ERROR: " ++)) allTs

-- | A temporary, hacky, indentation function. It should be removed when we
-- switch to using something else for error messages, which can be later
-- formatted nicely.
temporaryIndent :: String -> String -> String
temporaryIndent r ('\n' : s) = '\n' : (r ++ temporaryIndent r s)
temporaryIndent r (c    : s) = c : temporaryIndent r s
temporaryIndent _ []         = []
