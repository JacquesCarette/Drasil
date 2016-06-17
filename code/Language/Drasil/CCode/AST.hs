module Language.Drasil.CCode.AST where

type Variable = String

data Code = C Include [Method]

type Method = (MethodDecl, [Statement])

type Include = [Header]

data Header = StdLibHeader
            | StdIOHeader

data MethodDecl = MethodDecl Type Name [ArgsDecl]

data ArgsDecl = ArgsDecl Type Variable

type Name = String

data Type = IntType
          | VoidType
          | StrType
          | PtrType Type
          | DblType
          
data Statement = Assign Variable CodeExpr
               | If CodeExpr [Statement] (Maybe [Statement])
               | Return CodeExpr

data CodeExpr =  Var Variable
              | Int Integer
              | Dbl Double
              | Pow CodeExpr CodeExpr
              | Mult CodeExpr CodeExpr
              | Add CodeExpr CodeExpr
              | Div CodeExpr CodeExpr
              | Sub CodeExpr CodeExpr
              | Leq CodeExpr CodeExpr
              | Lt CodeExpr CodeExpr
              | Geq CodeExpr CodeExpr
              | Gt CodeExpr CodeExpr
              | Eq CodeExpr CodeExpr
              
data CodeType = Calc
data Lang = CLang

