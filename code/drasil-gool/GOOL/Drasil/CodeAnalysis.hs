module GOOL.Drasil.CodeAnalysis (
  Exception(loc, exc), printExc, hasLoc, exception, stdExc
) where

-- Stores exception information
-- Eq is needed so that the same exception doesn't get added multiple times 
-- to the list of exceptions for a given method.
data Exception = Exc {
  loc :: String, -- import string where the exception is defined
  exc :: String -- name of the exception
} deriving Eq

printExc :: Exception -> String
printExc (Exc l e) = if null l then e else l ++ "." ++ e

hasLoc :: Exception -> Bool
hasLoc (Exc [] _) = False
hasLoc _ = True

exception :: String -> String -> Exception
exception = Exc

stdExc :: String -> Exception
stdExc = Exc ""