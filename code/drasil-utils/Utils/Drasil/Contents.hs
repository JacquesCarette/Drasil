module Utils.Drasil.Contents (enumBullet, enumBulletU, enumSimple,
  enumSimpleU, eqUnR, eqUnR', mkEnumSimpleD) where

import Language.Drasil
import Utils.Drasil.Misc (bulletFlat, mkEnumAbbrevList)

import Control.Lens ((^.))

-- | Constructs 'LabelledContent' from an expression and a reference.
eqUnR :: Expr -> Reference -> LabelledContent
eqUnR e lbl = llcc lbl $ EqnBlock e

-- | Same as 'eqUnR' except content is unlabelled.
eqUnR' :: Expr -> Contents
eqUnR' e = UlC $ ulcc $ EqnBlock e

-- | Applies 'Enumeration', 'Bullet' and 'Flat' to a list.
enumBullet :: Reference -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumBullet lb s = llcc lb $ Enumeration $ bulletFlat s

-- | Same as 'enumBullet' but unlabelled.
enumBulletU :: [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumBulletU s =  UlC $ ulcc $ Enumeration $ bulletFlat s

-- | Enumerates a list and applies `Simple` and `Enumeration` to it:
--
--     * lb - Reference,
--     * s - start index for the enumeration,
--     * t - title of the list,
--     * l - list to be enumerated.
enumSimple :: Reference -> Integer -> Sentence -> [Sentence] -> LabelledContent --FIXME: should Enumeration be labelled?
enumSimple lb s t l = llcc lb $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | Same as 'enumSimple' but unlabelled.
enumSimpleU :: Integer -> Sentence -> [Sentence] -> Contents --FIXME: should Enumeration be labelled?
enumSimpleU s t l = UlC $ ulcc $ Enumeration $ Simple $ noRefsLT $ mkEnumAbbrevList s t l

-- | Converts lists of tuples containing a title ('Sentence') and 'ItemType' into
-- a 'ListTuple' which can be used with 'Contents' but not directly referable.
noRefsLT :: [(Sentence, ItemType)] -> [ListTuple]
noRefsLT a = uncurry zip3 (unzip a) $ repeat Nothing

-- | Convenience function for transforming types which are
-- instances of the constraints 'Referable', 'HasShortName', and 'Definition', into
-- Simple-type 'Enumeration's.
mkEnumSimpleD :: (Referable c, HasShortName c, Definition c) => [c] -> [Contents]
mkEnumSimpleD = mkEnumSimple $ mkListTuple (\x -> Flat $ x ^. defn)

-- | Convenience function for converting lists into
-- Simple-type 'Enumeration's.
mkEnumSimple :: (a -> ListTuple) -> [a] -> [Contents]
mkEnumSimple f = replicate 1 . UlC . ulcc . Enumeration . Simple . map f

-- | Creates a 'ListTuple', filling in the title with a 'ShortName' and filling
-- reference information.
mkListTuple :: (Referable c, HasShortName c) => (c -> ItemType) -> c -> ListTuple
mkListTuple f x = (getSentSN $ shortname x, f x, Just $ refAdd x)
