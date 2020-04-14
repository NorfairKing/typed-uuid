{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.UUID.Typed where

import Data.GenValidity
import Data.GenValidity.UUID ()
import Data.UUID.Typed
import Test.QuickCheck

instance GenUnchecked (UUID a) where
  genUnchecked = UUID <$> genUnchecked
  shrinkUnchecked (UUID u) = UUID <$> shrinkUnchecked u

instance GenValid (UUID a) where
  genValid = UUID <$> genValid
  shrinkValid (UUID u) = UUID <$> shrinkValid u

instance Arbitrary (UUID a) where
  arbitrary = genValid
