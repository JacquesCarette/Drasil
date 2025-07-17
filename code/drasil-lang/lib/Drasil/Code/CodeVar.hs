{-# LANGUAGE TemplateHaskell #-}
-- | Defines chunk types for use in code generation.
module Drasil.Code.CodeVar where

import Data.Char (isSpace)
import Control.Lens ((^.), view, makeLenses, Lens')

import Drasil.Code.Classes (Callable)
import Drasil.Code.CodeExpr.Lang (CodeExpr)
import Drasil.Database.UID (HasUID(uid), (+++))

import Language.Drasil.Classes (CommonIdea(abrv), Quantity, Idea(getA), NamedIdea(..), Definition (defn), ConceptDomain (cdom))
import Language.Drasil.Space (HasSpace(..), Space(..))
import Language.Drasil.Symbol (HasSymbol(symbol))
import Language.Drasil.Chunk.UnitDefn (MayHaveUnit(getUnit))
import Language.Drasil.Stages (Stage(..))

import Utils.Drasil (toPlainName)
import Language.Drasil.Chunk.Concept (dccAWDS)
import Language.Drasil.Chunk.DefinedQuantity (dqd', DefinedQuantityDict)
import Language.Drasil.Sentence (Sentence(S))

-- not using lenses for now
-- | A 'CodeIdea' must include some code and its name. 
class CodeIdea c where
  -- | Name of the idea.
  codeName  :: c -> String
  -- | Code chunk associated with the idea.
  codeChunk :: c -> CodeChunk

-- | A 'DefiningCodeExpr' must have it's underlying chunk 
--   defined in the CodeExpr language.
class CodeIdea c => DefiningCodeExpr c where
  codeExpr  :: Lens' c CodeExpr

-- | Convert an abbreviation into one deemed 'code-friendly', removing spaces,
--   and replacing special characters with underscores.
--
--   FIXME: This should NOT be treated as a 'getter', but something we cache
--   local to something that has a 'program name'.
programName :: CommonIdea c => c -> String
programName = toPlainName . filter (not . isSpace) . abrv

-- | Used when a function name needs to be distinguishable from a variable name.
funcPrefix :: String
funcPrefix = "func_"
 
-- | Details if a piece of code is meant to be a variable or a function.
data VarOrFunc = Var | Func

-- | Basic chunk representation in the code generation context.
-- Contains a QuantityDict and the kind of code (variable or function).
data CodeChunk = CodeC { _qc  :: DefinedQuantityDict
                       , kind :: VarOrFunc  -- TODO: Jason: Once we have function spaces, I believe we won't need to store this
                       }
makeLenses ''CodeChunk

-- | Finds the 'UID' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasUID        CodeChunk where uid = qc . uid
-- | Finds the term ('NP') of the 'QuantityDict' used to make the 'CodeChunk'.
instance NamedIdea     CodeChunk where term = qc . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'CodeChunk'.
instance Idea          CodeChunk where getA = getA . view qc
-- | Finds the Definition contained in the 'DefinedQuantityDict' used to make the CodeChunk
instance Definition    CodeChunk where defn = qc . defn

instance ConceptDomain CodeChunk where cdom = cdom . view qc
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasSpace      CodeChunk where typ = qc . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'QuantityDict' used to make the 'CodeChunk'.
instance HasSymbol     CodeChunk where symbol = symbol . view qc
-- | 'CodeChunk's have a 'Quantity'.
instance Quantity      CodeChunk
-- | Equal if 'UID's are equal.
instance Eq            CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'QuantityDict' used to make the 'CodeChunk'.
instance MayHaveUnit   CodeChunk where getUnit = getUnit . view qc

-- | Chunk representing a variable. The @obv@ field represents the object containing 
-- this variable, if it is an object field.
data CodeVarChunk = CodeVC {_ccv :: CodeChunk,
                            _obv :: Maybe CodeChunk}
makeLenses ''CodeVarChunk

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasUID        CodeVarChunk where uid = ccv . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance NamedIdea     CodeVarChunk where term = ccv . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeVarChunk'.
instance Idea          CodeVarChunk where getA = getA . view ccv

instance Definition    CodeVarChunk where defn = ccv . defn

instance ConceptDomain CodeVarChunk where cdom = cdom . view ccv
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasSpace      CodeVarChunk where typ = ccv . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance HasSymbol     CodeVarChunk where symbol = symbol . view ccv
-- | 'CodeVarChunk's have a 'Quantity'.
instance Quantity      CodeVarChunk
-- | Equal if 'UID's are equal.
instance Eq            CodeVarChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeVarChunk'.
instance MayHaveUnit   CodeVarChunk where getUnit = getUnit . view ccv

-- | Chunk representing a function.
newtype CodeFuncChunk = CodeFC {_ccf :: CodeChunk}
makeLenses ''CodeFuncChunk

-- | Finds the 'UID' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasUID        CodeFuncChunk where uid = ccf . uid
-- | Finds the term ('NP') of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance NamedIdea     CodeFuncChunk where term = ccf . term
-- | Finds the idea contained in the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance Idea          CodeFuncChunk where getA = getA . view ccf
-- | Finds the Definition of the 'CodeChunk' used to make the 'CodeFuncChunk'
instance Definition    CodeFuncChunk where defn = ccf . defn
-- | Finds the ConceptDomain of the 'CodeChunk' used to make the 'CodeFuncChunk'
instance ConceptDomain CodeFuncChunk where cdom = cdom . view ccf
-- | Finds the 'Space' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasSpace      CodeFuncChunk where typ = ccf . typ
-- | Finds the 'Stage' dependent 'Symbol' of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance HasSymbol     CodeFuncChunk where symbol = symbol . view ccf
-- | 'CodeFuncChunk's have a 'Quantity'.
instance Quantity      CodeFuncChunk
-- | Functions are Callable.
instance Callable      CodeFuncChunk
-- | Equal if 'UID's are equal.
instance Eq            CodeFuncChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)
-- | Finds the units of the 'CodeChunk' used to make the 'CodeFuncChunk'.
instance MayHaveUnit   CodeFuncChunk where getUnit = getUnit . view ccf

-- FIXME: use show for the UID here? Perhaps need a different implVar function for UIDs
-- Changes a 'CodeVarChunk'\'s space from 'Vect' to 'Array'.
listToArray :: CodeVarChunk -> CodeVarChunk
listToArray c = newSpc (c ^. typ)
  where newSpc (Vect t) = CodeVC (CodeC (dqd' (dccAWDS (show $ c +++ "_array")
          (c ^. term) (S "placeholder definition") (getA c)) 
          (const (symbol c Implementation)) (Array t) (getUnit c)) Var) (c ^. obv)
        newSpc _ = c
