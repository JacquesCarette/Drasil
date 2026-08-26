module Language.Drasil.Printing.Import.Literal (literal) where

import Language.Drasil (dbl)
import Language.Drasil.Literal.Development (Literal(..))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation,
  Notation(..), notation)

import Control.Lens ((^.))
import Numeric (floatToDigits)

literal :: Literal -> PrintingInformation -> P.Expr
literal (Dbl d)                  sm = case sm ^. notation of
  Engineering ->
     let (f, s) = processExpo $ snd $ floatToDigits 10 d in
     P.Row $ digitsProcess (map toInteger $ fst $ floatToDigits 10 d)
     f 0 (toInteger s)
  Scientific  ->  P.Dbl d
literal (Int i)                   _ = P.Int i
literal (ExactDbl d)              _ = P.Int d
literal (Str s)                   _ = P.Str s
literal (Perc a b)               sm = P.Row [literal (dbl val) sm, P.MO P.Perc]
  where
    val = fromIntegral a / (10 ** fromIntegral (b - 2))

-- | Processes the digits from the 'floatToDigits' function, decimal point
-- position, a counter, and exponent.
digitsProcess :: [Integer] -> Int -> Int -> Integer -> [P.Expr]
digitsProcess [0] _ _ _ = [P.Int 0, P.MO P.Point, P.Int 0]
digitsProcess ds pos _ (-3) = [P.Int 0, P.MO P.Point] ++ replicate (3 - pos) (P.Int 0) ++ map P.Int ds
digitsProcess (hd:tl) pos coun ex
  | pos /= coun = P.Int hd : digitsProcess tl pos (coun + 1) ex
  | ex /= 0 = [P.MO P.Point, P.Int hd] ++ map P.Int tl ++ [P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int hd] ++ map P.Int tl
digitsProcess [] pos coun ex
  | pos > coun = P.Int 0 : digitsProcess [] pos (coun+1) ex
  | ex /= 0 = [P.MO P.Point, P.Int 0, P.MO P.Dot, P.Int 10, P.Sup $ P.Int ex]
  | otherwise = [P.MO P.Point, P.Int 0]

-- | Takes the exponent and the 'Int' of the base and gives the decimal point
-- position and processed exponent. This function supports transferring
-- scientific notation to engineering notation.
--
-- References for standard of Engineering Notation:
-- * <https://www.khanacademy.org/science/electrical-engineering/introduction-to-ee/intro-to-ee/a/ee-numbers-in-electrical-engineering>
-- * <https://www.calculatorsoup.com/calculators/math/scientific-notation-converter.php>
-- * <https://en.wikipedia.org/wiki/Scientific_notation>
processExpo :: Int -> (Int, Int)
processExpo a = (r, a - r)
  where r = 1 + mod (a - 1) 3
