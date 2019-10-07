{-# LANGUAGE PostfixOperators #-}

-- | The structure for a class of renderers is defined here.
module GOOL.Drasil.Generic (
  construct, method
) where

import GOOL.Drasil.CodeType (CodeType(..))
import GOOL.Drasil.Symantics (Label,
  RenderSym(..), BodySym(..), PermanenceSym(..), TypeSym(..), ScopeSym(..),
  MethodTypeSym(mType), ParameterSym(..), MethodSym(Method), InternalMethod(..))
import GOOL.Drasil.Data (TypeData(..), td)

import Prelude hiding (break,print,last,mod,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text, empty, render, (<>), (<+>), ($+$),
  brackets, parens, isEmpty, rbrace, lbrace, vcat, char, double, quotes, 
  integer, semi, equals, braces, int, comma, colon, hcat)

construct :: Label -> TypeData
construct n = td (Object n) n empty

method :: (RenderSym repr) => Label -> Label -> repr (Scope repr) -> 
  repr (Permanence repr) -> repr (Type repr) -> 
  [repr (Parameter repr)] -> repr (Body repr) -> repr (Method repr)
method n c s p t = intMethod n c s p (mType t)