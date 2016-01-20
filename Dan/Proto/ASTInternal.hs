{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs #-}
module ASTInternal where

import Chunk (Chunk)

data Expr where
 V :: Variable -> Expr
 Dbl :: Double -> Expr
 Int :: Integer -> Expr
 (:^) :: Expr -> Expr -> Expr
 (:*) :: Expr -> Expr -> Expr
 (:/) :: Expr -> Expr -> Expr
 (:+) :: Expr -> Expr -> Expr
 (:-) :: Expr -> Expr -> Expr
 C :: Chunk c => c -> Expr

type Variable = String

data DocType = SRS
             | LPM
             | Code

data DocParams = DocClass String String --SqBracks vs. Braces
               | UsePackages [String] -- Package name list
               | ExDoc String String --SqBracks vs. Braces
