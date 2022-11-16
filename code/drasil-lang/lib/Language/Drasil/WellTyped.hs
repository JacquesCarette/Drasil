{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Drasil.WellTyped where

import qualified Data.Map.Strict as M
import Language.Drasil.UID (UID)

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

allOfType :: (Traversable tr, Typed e t) => TypingContext t -> tr e -> t -> Bool
allOfType cxt es t = foldr
  (\e acc -> acc && either (== t) (const False) (infer cxt e))
  True
  es
