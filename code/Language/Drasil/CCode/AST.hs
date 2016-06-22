module Language.Drasil.CCode.AST where

type Variable = String

data Code = C [Header] [VarDecl] [Method]

type Method = (MethodDecl, [Statement])

data Header = Library HeaderName
            | Local HeaderName

type HeaderName = String

data VarDecl = VarDecl Type Variable

data MethodDecl = MethodDecl Type Name [VarDecl]

--data ArgsDecl = ArgsDecl Type Variable

type Name = String

data Type = IntType
          | VoidType
          | StrType
          | PtrType Type
          | DblType

data Statement = Declare Type Variable (Maybe CodeExpr)
               | Assign Variable CodeExpr
               | If CodeExpr [Statement] (Maybe [Statement])
               | Print String
               | MethodCall Method [CodeExpr]
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

