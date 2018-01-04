{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.UUID.Typed where

import Data.GenValidity
import Data.GenValidity.UUID ()
import Data.UUID.Typed
import Test.QuickCheck

instance GenUnchecked (UUID a)

instance GenValid (UUID a)

instance Arbitrary (UUID a) where
    arbitrary = genValid
