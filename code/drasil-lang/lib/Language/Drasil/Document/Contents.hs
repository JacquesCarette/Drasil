-- | General functions that are useful in manipulating some Drasil types into
-- printable 'Contents'.
module Language.Drasil.Document.Contents (
  -- * List Creation Functions
  enumBullet, enumBulletU, enumSimple,
  enumSimpleU, mkEnumSimpleD,

  -- * Displaying Expressions
  lbldExpr, unlbldExpr,
  unlbldCode
) where

import Control.Lens ((^.))

import Drasil.Code.CodeExpr.Lang (CodeExpr)
import Language.Drasil.Classes (Definition(..))
import Language.Drasil.ShortName (HasShortName(..), getSentSN)
import Language.Drasil.Document (llcc, ulcc)
import Language.Drasil.Document.Core
    (LabelledContent,
     RawContent(Enumeration, EqnBlock, CodeBlock),
     Contents(UlC),
     ListTuple,
     ItemType(Flat),
     ListType(Simple))
import Language.Drasil.ModelExpr.Lang (ModelExpr)
import Language.Drasil.Reference (Reference)
import Language.Drasil.Sentence (Sentence (..))
import Language.Drasil.Document.Combinators (bulletFlat, mkEnumAbbrevList)
import Language.Drasil.Label.Type ( Referable(refAdd) )

-- | Displays a given expression and attaches a 'Reference' to it.
lbldExpr :: ModelExpr -> Reference -> LabelledContent
lbldExpr c lbl = llcc lbl $ EqnBlock c

-- | Same as 'eqUnR' except content is unlabelled (does not attach a 'Reference').
unlbldExpr :: ModelExpr -> Contents
unlbldExpr c = UlC $ ulcc $ EqnBlock c

-- | Unlabelled code expression
unlbldCode :: CodeExpr -> Contents
unlbldCode c = UlC $ ulcc $ CodeBlock c

-- | Creates a bulleted list.
enumBullet :: Reference -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumBullet lb s = llcc lb $ Enumeration $ bulletFlat s

-- | Same as 'enumBullet' but unlabelled.
enumBulletU :: [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumBulletU s =  UlC $ ulcc $ Enumeration $ bulletFlat s

-- | Currently Unused. Creates a simple bulleted list that labels things with
-- a title and number:
--
--     * lb - Reference,
--     * s - start index for the enumeration,
--     * t - title of the list,
--     * l - list to be enumerated.
--
-- For example, if we want to create a list of data definitions, we could call the function as follows:
--
-- > enumSimple _ 2 (S "DD") [def1, def2, ...]
--
-- And the resulting 'LabelledContent' would be rendered as:
--
--     * DD2: def1
--     * DD3: def2
--     * DD4: def3 ...
enumSimple :: Reference -> Integer -> Sentence -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumSimple lb s t l = llcc lb $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | Same as 'enumSimple' but unlabelled.
enumSimpleU :: Integer -> Sentence -> [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumSimpleU s t l = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | Converts lists of tuples containing a title ('Sentence') and 'ItemType' into
-- a bulleted list (['ListTuple']) which can be used with 'Contents' but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing

-- | Convenience function for transforming referable concepts into a bulleted list.
-- Used in drasil-docLang in making the assumptions, goals, and requirements sections.
-- Output is of the kind @Concept Name: definition of concept@.
mkEnumSimpleD :: (Referable c, HasShortName c, Definition c) => [c] -> [Contents]
mkEnumSimpleD = mkEnumSimple $ mkListTuple (\x -> Flat $ x ^. defn)

-- | Helper function for converting a list of something into a bulleted list.
-- Used in 'mkEnumSimpleD'.
mkEnumSimple :: (a -> ListTuple) -> [a] -> [Contents]
mkEnumSimple _ [] = []
mkEnumSimple f xs = [UlC $ ulcc $ Enumeration $ Simple $ map f xs]

-- | Helper function that creates a bullet point from a function and an item.
-- Used in 'mkEnumSimpleD'.
mkListTuple :: (Referable c, HasShortName c) => (c -> ItemType) -> c -> ListTuple
mkListTuple f x = (getSentSN $ shortname x, f x, Just $ refAdd x)
