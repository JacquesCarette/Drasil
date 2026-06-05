{-# LANGUAGE PostfixOperators #-}
module OOVector (ooVector) where

import Drasil.GOOL
import Prelude hiding (return,print,log,exp,sin,cos,tan)

ooVector :: OOProg r => GSProgram r
ooVector = prog "OOVector" "" [fileDoc (buildModule "OOVector" []
  [main] [vectorClass])]

vectorClass :: OOProg r => SClass r
vectorClass = docClass "Vectors of doubles and common vector-related operations." $
  extraClass "Vector" Nothing [stateVar private instanceLevel localV]
  [docFunc "Construct a vector from an array of doubles." ["The doubles."] Nothing $
   constructor [param argV] [] (body [
     block [
       assert (arrayLength (valueOf localV) ?> litInt 0)
              (litString "Vector dimension must be > 0."),
       thisV &= arrayCopy (valueOf argV)
     ]
   ])]
  methods
  where argV = var "v" (arrayType double)
        methods = [dimension, magnitude, norm, dot, add, scale, print_]

vecVar :: (VariableSym r, OOTypeSym r) => String -> SVariable r
vecVar v = var v (obj "Vector")

localV :: VariableSym r => SVariable r
localV = var "v" (arrayType double)

thisV :: InstanceVarSelfSym r =>SVariable r
thisV = instanceVarSelf localV

dimension :: OOProg r => SMethod r
dimension = docFunc "Returns the dimension of this vector." [] (Just "The dimension of the vector.") $
  method "dimension" public instanceLevel int [] $ bodyStatements [
    returnStmt $ arrayLength (valueOf thisV)
  ]

magnitude :: OOProg r => SMethod r
magnitude = docFunc "Calculate the Euclidean norm (magnitude) of this vector."
  [] (Just "The magnitude.") $
  pubMethod "magnitude" double [] $ oneLiner $
    returnStmt (classMethodCall double (obj "Vector") "dot" [valueOf self, valueOf self] #/^)

norm :: OOProg r => SMethod r
norm = docFunc "Calculate unit vector of this vector."
  [] (Just "A new unit vector.") $
  pubMethod "norm" (obj "Vector") [] $ bodyStatements [
    varDecDef mag local (objMethodCall double (valueOf self) "magnitude" []),
    assert (valueOf mag ?> litDouble 0.0) (litString "Cannot normalize a zero vector."),
    returnStmt (objMethodCall (obj "Vector") (valueOf self) "scale" [litDouble 1.0 #/ valueOf mag])
  ]
  where mag = var "mag" double

dot :: OOProg r => SMethod r
dot = docFunc "Calculate the dot product of two vectors."
  ["First vector.", "Second vector."] (Just "The dot product.") $
  method "dot" public classLevel double [param v1, param v2] $ bodyStatements [
    assert (objMethodCallNoParams int (valueOf v1) "dimension" ?==
            objMethodCallNoParams int (valueOf v2) "dimension")
           (litString "Vector dimensions must match for dot product."),
    varDecDef res local (litDouble 0.0),
    forRange i (litInt 0) (objMethodCallNoParams int (valueOf v1) "dimension") (litInt 1) (bodyStatements [
      res &+= (listAccess (valueOf (instanceVarAccess (valueOf v1) localV)) (valueOf i) #*
                             listAccess (valueOf (instanceVarAccess (valueOf v2) localV)) (valueOf i))
    ]),
    returnStmt $ valueOf res
  ]
  where v1 = vecVar "v1"
        v2 = vecVar "v2"
        res = var "res" double
        i = var "i" int

add :: OOProg r => SMethod r
add = docFunc "Calculate the resultant vector of two vectors."
  ["First vector.", "Second vector."] (Just "The resultant vector.") $
  method "add" public classLevel (obj "Vector") [param v1, param v2] $ bodyStatements [
    assert (objMethodCallNoParams int (valueOf v1) "dimension" ?==
            objMethodCallNoParams int (valueOf v2) "dimension")
           (litString "Vector dimensions must match for addition."),
    varDecDef res local (arrayCopy (valueOf $ instanceVarAccess (valueOf v1) localV)),
    forRange i (litInt 0) (objMethodCallNoParams int (valueOf v1) "dimension") (litInt 1) (bodyStatements [
      arrayElem (valueOf i) res &+= listAccess (valueOf (instanceVarAccess (valueOf v2) localV)) (valueOf i)
    ]),
    returnStmt (newObj (obj "Vector") [valueOf res])
  ]
  where v1 = vecVar "v1"
        v2 = vecVar "v2"
        res = var "res" (arrayType double)
        i = var "i" int

scale :: OOProg r => SMethod r
scale = docFunc "Scale this vector by a factor."
  ["Scalar factor."] (Just "A new scaled vector.") $
  pubMethod "scale" (obj "Vector") [param s] $ bodyStatements [
    varDecDef res local (arrayCopy (valueOf thisV)),
    forRange i (litInt 0) (objMethodCallNoParams int (valueOf self) "dimension") (litInt 1) (bodyStatements [
      arrayElem (valueOf i) res &= (valueOf s #* listAccess (valueOf res) (valueOf i))
    ]),
    returnStmt (newObj (obj "Vector") [valueOf res])
  ]
  where s = var "s" double
        res = var "res" (arrayType double)
        i = var "i" int

print_ :: OOProg r => SMethod r
print_ = docFunc "Prints the vector elements to console." [] Nothing $
  pubMethod "print" void [] $ oneLiner $ printLn $ valueOf thisV

main :: OOProg r => SMethod r
main = mainFunction $ body [
    block [
      arrayDecDef ds1 mainFn [litDouble 1.0, litDouble 2.0, litDouble 3.0],
      arrayDecDef ds2 mainFn [litDouble 4.0, litDouble 5.0, litDouble 6.0],
      varDecDef v1 mainFn $ newObj (obj "Vector") [valueOf ds1],
      varDecDef v2 mainFn $ newObj (obj "Vector") [valueOf ds2],

      printStr "v1: ", valStmt $ objMethodCallNoParams void (valueOf v1) "print",
      printStr "v2: ", valStmt $ objMethodCallNoParams void (valueOf v2) "print",

      varDecDef (var "d" double) mainFn (classMethodCall (obj "Vector") (obj "Vector") "dot" [valueOf v1, valueOf v2]),
      printStr "Dot product: ", printLn (valueOf $ var "d" double),

      varDecDef (var "m" double) mainFn (objMethodCallNoParams double (valueOf v1) "magnitude"),
      printStr "Magnitude of v1: ", printLn (valueOf $ var "m" double),

      varDecDef (var "vAdd" (obj "Vector")) mainFn (classMethodCall (obj "Vector") (obj "Vector") "add" [valueOf v1, valueOf v2]),
      printStr "v1 + v2: ", valStmt $ objMethodCallNoParams void (valueOf (var "vAdd" (obj "Vector"))) "print",

      varDecDef (var "vUnit" (obj "Vector")) mainFn (objMethodCall (obj "Vector") (classMethodCall (obj "Vector") (obj "Vector") "add" [valueOf v1, objMethodCall (obj "Vector") (valueOf v2) "scale" [litDouble 2]]) "norm" []),
      printStr "Unit vector of v1 + 2 * v2: ", valStmt $ objMethodCallNoParams void (valueOf (var "vUnit" (obj "Vector"))) "print"
    ]
  ]
  where ds1 = var "ds1" (arrayType double)
        ds2 = var "ds2" (arrayType double)
        v1 = vecVar "v1"
        v2 = vecVar "v2"
