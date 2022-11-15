{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Drasil.WellTyped where

import qualified Data.Map.Strict as M
-- import qualified Data.Text as T
import Language.Drasil.UID (UID)

type TypeError = String -- T.Text

type TypingContext t = M.Map UID t

-- data WellTyped e t = WellTyped
--   { _e :: e,
--     _t :: t
--   }

inferFromContext :: TypingContext t -> UID -> Either t TypeError
inferFromContext cxt u = maybe
    (Right $ "`" ++ show u ++ "` lacks type binding in context")
    Left
    (M.lookup u cxt)

check :: Typed e t => TypingContext t -> e -> t -> Either t TypeError
check cxt e t = either
    (\infT -> if t == infT then Left t else Right $ "Type `" ++ show t ++ "` does not match expected type `" ++ show infT ++ "`")
    (\te -> Right $ "Input is ill-typed, error: " ++ te)
    (infer cxt e)

class (Eq t, Show t) => Typed e t where
  infer :: TypingContext t -> e -> Either t TypeError
--   check :: TypingContext t -> e -> t -> Either t TypeError

class Typed e t => TypeChecks c e t where
  typeCheckExpr :: c -> (e, t)

allOfType :: (Traversable tr, Typed e t) => TypingContext t -> tr e -> t -> Bool
allOfType cxt es t = foldr (\e acc -> acc && either (== t) (const False) (infer cxt e)) True es
