{-# OPTIONS -Wall #-} 
module ASTCode where

type Variable = String

data Code = C [Method]
type Method = (Declaration, [Statement])

data Declaration = MethDecl Type Name [Declaration]
                 | ArgDecl Type Variable
type Name = String

data Type = IntType
          | VoidType
          | StrType
          | PtrType Type
          | DblType
          
data Statement = Return CodeExpr --Currently the only necessary statement type.

data CodeExpr =  Var Variable
              | Int Integer
              | Dbl Double
              | Pow CodeExpr CodeExpr
              | Mult CodeExpr CodeExpr
              | Add CodeExpr CodeExpr
              | Div CodeExpr CodeExpr
              | Sub CodeExpr CodeExpr
              
data CodeType = Calc
data Lang = CLang