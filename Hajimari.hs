-- Custom Prelude for my projects.

-- We leave all the actual names here, instead of
-- just using .. so that someone reading knows exactly
-- what's exported.
module Hajimari
  -- Basic data types.
  ( Bool(True, False), (&&), (||), not, otherwise
  , Maybe(Nothing, Just), maybe
  , Either(Left, Right), either
  , Ordering(LT, EQ, GT)
  , fst, snd, curry, uncurry
  -- Strings and Text.
  , ByteString, Text
  -- Some basic typeclasses.
  , Eq((==), (/=))
  , Ord(compare, (<), (<=), (>=), (>), max, min)
  , Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo)
  , Bounded(minBound, maxBound)
  , Countable(length)
  , Null(null)
  , Reversable(reverse)
  -- Numbers.
  , Int, Integer, Word
  , Float, Double, Rational
  , Num((+), (-), (*), negate, abs, signum, fromInteger)
  , Real(toRational)
  , Integral(quot, rem, div, mod, quotRem, divMod, toInteger)
  , Fractional((/), recip, fromRational)
  , Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh)
  , RealFrac(properFraction, truncate, round, ceiling, floor)
  , RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2)
  , subtract, even, odd, gcd, lcm, (^), (^^), fromIntegral, realToFrac
  -- Monoids, monads, applicatives, functors.
  , Monoid(mempty, mappend, mconcat)
  , Functor(fmap, (<$)), (<$>)
  , Applicative(pure, (<*>), (*>), (<*))
  , Monad((>>=), (>>), return, fail)
  , (=<<)
  -- Folds and traversals.
  , Foldable(elem, foldMap, foldr, foldl, foldr1, foldl1, maximum, minimum,
             product, sum)
  , Traversable(traverse, sequenceA, mapM, sequence)
  -- Miscellaneous functions.
  , id, const, (.), flip, ($)
  , undefined
  , seq, ($!) -- Careful with these. Use when strictness is required.
  -- Special folds and list traverals
  , and, or, any, all
  , scanl, scanl1, scanr, scanr1
  -- Infinite lists
  , iterate, repeat, replicate, cycleMay, cycleSafe
  -- Sublists
  , take, drop, splitAt, takeWhile, dropWhile, span, break
  -- Searching
  , notElem, lookup
  -- Zipping/unzipping lists
  , zip, zip3, zipWith, zipWith3, unzip, unzip3
  -- String functions
  , lines, words, unlines, unwords
  -- Show and Read (converting to and from strings)
  )
where

-- We've tried to unify the interfaces across Lists, ByteStrings,
-- and Text as best we can. However, some just aren't unifiable
-- because List is a type constructor and ByteStrings and Text aren't,
-- so the type signatures don't work out.

-- `length', `null', and `reverse' will work across all three.
-- With the others, we prioritized exporting the list versions from
-- Prelude.

import Prelude hiding (length, null, reverse)
import Data.ByteString.Lazy hiding 
  ( length
  , null
  , reverse
  , repeat, replicate, iterate
  , scanl
  , any, all
  , take, drop, splitAt, takeWhile, dropWhile, span, break
  , notElem
  , zip, zipWith, unzip
  )
import Data.Text.Lazy hiding 
  ( length
  , null
  , reverse
  , repeat, replicate, iterate
  , scanr, scanr1, scanl, scanl1
  , any, all
  , take, drop, splitAt, takeWhile, dropWhile, span, break
  , zip, zipWith
  , lines, words, unlines, unwords
  )

import qualified Prelude as Prelude 
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T 

class Countable a where
  length :: Integral b => a -> b

instance Countable [a] where
  length = fromIntegral . Prelude.length
instance Countable ByteString where
  length = fromIntegral . BS.length
instance Countable Text where
  length = fromIntegral . T.length

class Null a where
  null :: a -> Bool

instance Null [a] where
  null = Prelude.null
instance Null ByteString where
  null = BS.null
instance Null Text where
  null = T.null

class Reversable a where
  reverse :: a -> a

instance Reversable [a] where
  reverse = Prelude.reverse
instance Reversable ByteString where
  reverse = BS.reverse
instance Reversable Text where
  reverse = T.reverse

class Repeatable a where
  cycleMay :: a -> Maybe a
  cycleSafe :: a -> a

instance Repeatable [a] where
  cycleMay [] = Nothing
  cycleMay l  = Just $ Prelude.cycle l
  cycleSafe [] = []
  cycleSafe l  = Prelude.cycle l
instance Repeatable ByteString where
  cycleMay s | null s    = Nothing
             | otherwise = Just $ BS.cycle s
  cycleSafe s | null s    = BS.empty
              | otherwise = BS.cycle s
instance Repeatable Text where
  cycleMay s | null s    = Nothing
             | otherwise = Just $ T.cycle s
  cycleSafe s | null s    = T.empty
              | otherwise = T.cycle s
