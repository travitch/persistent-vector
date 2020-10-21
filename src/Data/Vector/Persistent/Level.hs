{-# language Trustworthy #-}

module Data.Vector.Persistent.Level
  ( Level (..)
  , SLevel (SZero, SFivePlus) -- The true constructor must not be exported for safety
  , unSLevel
  , minusFive
  , Compared (..)
  , cmpLevel
  , CheckFive (..)
  , checkFive
  ) where

import Data.Vector.Persistent.Level.Unsafe
