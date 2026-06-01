module Drasil.Shared.CodeAnalysis (
  ExceptionType(..), Exception(loc, exc), printExc, hasLoc, exception, stdExc,
  HasException(..)
) where

-- Eq is needed so that the same exception doesn't get added multiple times
-- to the list of exceptions for a given method.
data ExceptionType = Standard | FileNotFound | IO deriving Eq

-- Stores exception information
data Exception = Exc {
  loc :: String, -- import string where the exception is defined
  exc :: String -- name of the exception
}

printExc :: Exception -> String
printExc (Exc l e) = if null l then e else l ++ "." ++ e

hasLoc :: Exception -> Bool
hasLoc (Exc [] _) = False
hasLoc (Exc (_:_) _) = True

exception :: String -> String -> Exception
exception = Exc

stdExc :: String -> Exception
stdExc = Exc ""

class HasException r where
  toConcreteExc :: ExceptionType -> r Exception
