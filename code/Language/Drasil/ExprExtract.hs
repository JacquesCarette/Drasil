module Language.Drasil.ExprExtract (egetDoc)where

import Control.Lens ((^.))
import Language.Drasil.Document
import Language.Drasil.Chunk.Eq (QDefinition)
import Language.Drasil.Unit(UnitDefn)
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Expr
import Language.Drasil.Chunk.ShortName
import Language.Drasil.Classes (HasUID(uid),NamedIdea(term), Idea(getA),
  HasSymbol(symbol), IsUnit, ExprRelat(relat), HasDerivation(derivations), 
  HasReference(getReferences), ConceptDomain, Definition(defn))

egetDoc :: Document -> [Expr]
egetDoc (Document _ _ s) = concatMap egetSec s

-- If collected sentences are used for collecting symbols, 
-- section collection has to avoid Reference Material section (named "RefMat"),
-- because this section includes Table of symbol which would
-- cause loop in collecting.

-- Auxiliary Constants Section (named "AuxConstants") contains standard 
-- values (like min, max) that are used for defined basic Chunk.
-- These values should not appear in the basic Table of symbol.
egetSec :: Section -> [Expr]
egetSec (Section _ _ _ (ShortNm "RefMat")) = []
egetSec (Section _ sc _ _) = concatMap egetSecCon sc
egetSec (Section _ _ _ _) = []

egetSecCon :: SecCons -> [Expr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon c

egetCon :: Contents -> [Expr]
egetCon (EqnBlock e _) = [e]
egetCon (Definition d) = egetDtype d 
egetCon (Defnt dt (hd:fs) a) = (concatMap egetCon $ snd hd) ++ egetCon (Defnt dt fs a)
egetCon (Defnt dt [] a) = []
egetCon _ = []

egetDtype :: DType -> [Expr]
egetDtype (Data q) = egetQDef q
egetDtype (Theory t) = [(t ^. relat)]
egetDtype _ = []

egetQDef :: QDefinition -> [Expr]
egetQDef a = [(a ^. relat)]

               

