-- | Implementation of a program in Abstract Code
module GOOL.CodeGeneration.AbstractCode.AbstractCodeImplementation (
    -- * Generated Class Names
    clsNames,

    -- * Constructing 'AbstractCode'
    makeAbstractCode
) where

import Prelude hiding (break,log,print,return)

import GOOL.CodeGeneration.AbstractCode
import GOOL.Auxil.DataTypes
import GOOL.Auxil.Helper (makeVarNameValid)

-- | Creates AbstractCode for the program
makeAbstractCode :: AbstractCode
makeAbstractCode = AbsCode $ Pack "AbsCodeTestPack" [circleClass, testClass] $ Payload []

-- Class names required globally
circleClassName, testName :: String
circleClassName = "Circle"
testName = "Test"

clsNames :: [String]
clsNames = [circleClassName, testName]

-- Helper/Convenience
circObj :: StateType
circObj = Type circleClassName

-- Variable names required globally
-- [none]

-- Function names required globally
-- [none]

-- implementation of the various classes
circleClass :: Class
circleClass =
    let modName = circleClassName
        radius = "radius"
        diam = "diameter"
        modVars = [
            StateVar radius Private Dynamic (Base Float) 0]
    in Class modName Nothing Public modVars [
        Method modName Public (Construct modName)
            [StateParam "rad" $ Base Float]
            [ Block [
                AssignState $ Assign (ObjVar Self (Var radius)) (Var "rad")
            ], Block [
                PrintState False (Base String) $ Lit $ LitStr "Circle created with radius ",
                PrintState True (Base Float) $ ObjVar Self (Var radius)
            ] ],
        Method "getDiameter" Public (MState $ Base Float)
            []
            [ Block [
                DeclState $ VarDecDef diam (Base Float) (Expr $ BinaryExpr (Var radius) Multiply (Lit $ LitFloat 2.0)),
                RetState $ Ret $ Var diam
            ] ],
        Method "getArea" Public (MState $ Base Float) [] $ oneLiner $
                RetState $ Ret $
                    Expr $ BinaryExpr (Lit $ LitFloat 3.14159)
                                      Multiply
                                      (Expr $ BinaryExpr (Var radius) Power (Lit $ LitInt 2))
    ]

testClass :: Class
testClass =
    let modName = testName
    in MainClass modName [] [
        MainMethod [ Block [
            objDecDef "circ" circObj (litObj circObj [litFloat 3.0])
        ] ]
    ]
