{-# language DataKinds, TypeFamilies, GADTs, ScopedTypeVariables #-}
{-# language Unsafe #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language RoleAnnotations #-}
{-# language BangPatterns #-}
{-# language TypeInType #-}
{-# OPTIONS_GHC -ddump-simpl #-}
-- |
-- Type-level numbers representing multiples of five
-- along with efficiently-represented singletons for
-- working with them.
module Data.Vector.Persistent.Level.Unsafe
  ( Level (..)
  , SLevel (.., SZero, SFivePlus)
  , unSLevel
  , minusFive
  , CheckFive (..)
  , checkFive
  , cmpLevel
  , Compared (..)
  ) where
  
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception (assert)
import Data.Bits (finiteBitSize)
import Data.Type.Equality
import GHC.Exts (TYPE)

data Level = Zero | FivePlus Level

newtype SLevel (n :: Level) = SLevel Int
  deriving (Show)
type role SLevel nominal

unSLevel :: SLevel n -> Int
unSLevel (SLevel n) = n

instance TestEquality SLevel where
  {-# INLINE testEquality #-}
  testEquality (SLevel a) (SLevel b)
    | a == b = Just (unsafeCoerce Refl)
    | otherwise = Nothing

data Compared (a :: Level) (b :: Level) where
  LevelLT :: Compared a b
  LevelEQ :: Compared a a
  LevelGT :: Compared a b

cmpLevel :: SLevel a -> SLevel b -> Compared a b
cmpLevel (SLevel a) (SLevel b) = case compare a b of
  LT -> LevelLT
  EQ -> unsafeCoerce LevelEQ
  GT -> LevelGT

-- Huh? Bear with me. We generally don't want to generate code to check for
-- level overflow, since that should never ever happen in context.  However, I
-- want to be able to offer a Trustworthy module, which needs to be really
-- quite sure.  On a 32-bit system, it would be quite easy to (perhaps
-- intentionally) produce overflow. On a 64-or-so-bit system, this will take a
-- *very long time* (perhaps thousands of years), so it seems reasonable to say
-- it just can't ever happen.
fivePlus :: SLevel n -> SLevel ('FivePlus n)
fivePlus (SLevel n)
  | finiteBitSize n >= 63
  = assert (n <= maxBound - 5) $ SLevel (n + 5)
  | n > maxBound - 5
  = error "Level overflow"
  | otherwise
  = SLevel (n + 5)

-- Is this really necessary?
minusFive :: SLevel ('FivePlus n) -> SLevel n
minusFive (SLevel n)
  = assert (n >= 10) $ SLevel (n - 5)

data CheckFive (n :: Level) where
  IsZero :: CheckFive 'Zero
  IsFivePlus :: !(SLevel m) -> CheckFive ('FivePlus m)

checkFive :: SLevel n -> CheckFive n
checkFive (SLevel 0) = unsafeCoerce IsZero
checkFive (SLevel n)
  = assert (n >= 5) $ unsafeCoerce $ IsFivePlus (SLevel (n - 5))
-- Staged inline for RULES
{-# INLINE [1] checkFive #-}

checkFiveSurePositive :: SLevel ('FivePlus n) -> CheckFive ('FivePlus n)
checkFiveSurePositive = IsFivePlus . minusFive
{-# INLINE checkFiveSurePositive #-}

checkFiveSureZero :: SLevel 'Zero -> CheckFive 'Zero
checkFiveSureZero !_ = IsZero
{-# INLINE checkFiveSureZero #-}

-- These rules avoid unnecessary equality tests when the types
-- show that the value is definitely zero or definitely positive.
-- This weird lambda-with-bang-pattern thing seems to be necessary
-- to convince the worker/wrapper transformation to work right.
-- Don't ask me why.
{-# RULES
"checkFive/Zero" checkFive = \ !x -> checkFiveSureZero x
"checkFive/Positive" checkFive = \ !x -> checkFiveSurePositive x
 #-}

pattern SZero :: () => n ~ 'Zero => SLevel n
pattern SZero <- (checkFive -> IsZero)
  where
    SZero = SLevel 0

pattern SFivePlus :: () => (n ~ 'FivePlus m) => SLevel m -> SLevel n
pattern SFivePlus n <- (checkFive -> IsFivePlus n)
  where
    SFivePlus = fivePlus

{-# COMPLETE SZero, SFivePlus #-}


--sLevel :: forall rep (r :: TYPE rep) n. (n ~ 'Zero => r) -> (forall m. n ~ 'FivePlus m => SLevel m -> r) -> SLevel n -> r
--sLevel z s l = case checkFive 
