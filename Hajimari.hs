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
  )
where

import Prelude hiding (length, null, reverse)
import Data.ByteString.Lazy hiding (length, null, reverse)
import Data.Text.Lazy hiding (length, null, reverse)

import qualified Prelude as Prelude (length, null, reverse)
import qualified Data.ByteString.Lazy as ByteString 
  ( ByteString
  , length
  , null
  , reverse
  )
import qualified Data.Text.Lazy as Text 
  ( Text
  , length
  , null
  , reverse
  )

class Countable a where
  length :: Integral b => a -> b

instance Countable [a] where
  length = fromIntegral . Prelude.length
instance Countable ByteString.ByteString where
  length = fromIntegral . ByteString.length
instance Countable Text.Text where
  length = fromIntegral . Text.length

class Null a where
  null :: a -> Bool

instance Null [a] where
  null = Prelude.null
instance Null ByteString.ByteString where
  null = ByteString.null
instance Null Text.Text where
  null = Text.null

class Reversable a where
  reverse :: a -> a

instance Reversable [a] where
  reverse = Prelude.reverse
instance Reversable ByteString.ByteString where
  reverse = ByteString.reverse
instance Reversable Text.Text where
  reverse = Text.reverse
