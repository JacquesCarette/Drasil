{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Language.Drasil.WellTyped where

import qualified Data.Map.Strict as M
import Language.Drasil.UID (UID)
import Data.List (intercalate)

-- TODO: TypeError should be a Doc type instead, so that we can have cleaner
-- error messages and formatting.
type TypeError = String

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
  | otherwise  = Right $ s ++ "\n    Received:\n" ++ dumpAllTs
  where
    allTs = map (infer cxt) es
    allTsAreSp = all (\case
      Left  t -> t == expect
      Right _ -> False) allTs
    -- FIXME: If an error is embedded within an error, then it will display
    -- poorly because of hard-coded spacing. We need Docs instead of Strings!
    dumpAllTs = intercalate "\n" $ map (("      - " ++) . either show ("ERROR: " ++)) allTs
