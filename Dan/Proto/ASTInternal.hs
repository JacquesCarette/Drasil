{-# OPTIONS -Wall #-} 
{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, AllowAmbiguousTypes #-}
module ASTInternal where

--Supported output formats for documentation.
data OutLang   = CLang

data Expr c = V Variable
          | Dbl Double
          | Int Integer
          | Expr c :^ Expr c
          | Expr c :* Expr c
          | Expr c :/ Expr c
          | Expr c :+ Expr c
          | Expr c :- Expr c
          | C c

type Variable = String

data FormatC = Hat
            | Vector
            | Grave
            | Acute
  deriving (Eq, Ord)
  

               
--data Context = Pg | Eqn | Cd -- paragraph, equation, or code
-- ----------------------------------------------------------------
-- data CodeType = Calc
-- data Precision = Single | Double

data DocType = SRS
             | LPM
             | Code

data DocParams = DocClass String String --SqBracks vs. Braces
               | UsePackages [String] -- Package name list
               | ExDoc String String --SqBracks vs. Braces
