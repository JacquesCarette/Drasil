module Language.Drasil.Code.CodeTest where

import Prelude hiding (return)
import Language.Drasil.Code.Imperative.AST
import Language.Drasil.Code.Imperative.LanguageRenderer(Options(..))

opt :: Options
opt = Options Nothing Nothing Nothing (Just "hghc")

-- | Creates AbstractCode for the program
makeAbstractCode :: AbstractCode
makeAbstractCode = AbsCode $ Pack "hghc" [hghcClass]

-- Class names required globally
hghcClassName :: String
hghcClassName = "hghc"

clsNames :: [String]
clsNames = [hghcClassName]

-- Helper/Convenience
hghcObj :: StateType
hghcObj = Type hghcClassName

-- Variable names required globally
-- [none]

-- Function names required globally
calc_hg, calc_hc :: String
calc_hg = "calc_hg"
calc_hc = "calc_hc"

-- IR (intermediate representation) of the various modules
hghcClass :: Class
hghcClass =
    let modName = hghcClassName
        k_c = "k_c"
        h_p = "h_p"
        h_b = "h_b"
        tau_c = "tau_c"
    in pubClass modName noParent [] [
        pubMethod (typ float) calc_hg [param k_c float, param h_p float, param tau_c float] [
            Block [ return $ Expr $ BinaryExpr (Expr $ BinaryExpr (Expr $ BinaryExpr (Lit $ LitFloat 2.0) Multiply (Var k_c)) Multiply (Var h_p)) Divide
             (Expr $ BinaryExpr (Expr $ BinaryExpr (Lit $ LitFloat 2.0) Multiply (Var k_c)) Plus (Expr $ BinaryExpr (Var tau_c) Multiply (Var h_p)))]
        ],
        pubMethod (typ float) calc_hc [param k_c float, param h_b float, param tau_c float] [
            Block [ return $ Expr $ BinaryExpr (Expr $ BinaryExpr (Expr $ BinaryExpr (Lit $ LitFloat 2.0) Multiply (Var k_c)) Multiply (Var h_b)) Divide
             (Expr $ BinaryExpr (Expr $ BinaryExpr (Lit $ LitFloat 2.0) Multiply (Var k_c)) Plus (Expr $ BinaryExpr (Var tau_c) Multiply (Var h_b)))]
        ]
    ]
