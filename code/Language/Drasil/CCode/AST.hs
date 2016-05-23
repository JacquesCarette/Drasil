module Language.Drasil.CCode.AST where

type Variable = String

data Code = C [Method]
type Method = (MethodDecl, [Statement])

data MethodDecl = MethodDecl Type Name [ArgsDecl]

data ArgsDecl = ArgsDecl Type Variable

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

