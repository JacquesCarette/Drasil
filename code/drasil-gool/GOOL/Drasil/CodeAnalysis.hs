module GOOL.Drasil.CodeAnalysis (
  Exception(loc, exc), printExc, hasLoc, exception, stdExc
) where

-- Stores exception information
data Exception = Exc {
  loc :: String, -- import string where the exception is defined
  exc :: String -- name of the exception
}

-- This is needed so that the same exception doesn't get added multiple times 
-- to the list of exceptions for a given method.
instance Eq Exception where
  (Exc l1 e1) == (Exc l2 e2) = l1 == l2 && e1 == e2

printExc :: Exception -> String
printExc (Exc l e) = if null l then e else l ++ "." ++ e

hasLoc :: Exception -> Bool
hasLoc (Exc [] _) = False
hasLoc _ = True

exception :: String -> String -> Exception
exception = Exc

stdExc :: String -> Exception
stdExc = Exc ""