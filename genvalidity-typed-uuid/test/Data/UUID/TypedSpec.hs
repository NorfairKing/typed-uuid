{-# LANGUAGE TypeApplications #-}

module Data.UUID.TypedSpec
    ( spec
    ) where

import Test.Hspec
import Test.Validity
import Test.Validity.Aeson

import Data.GenValidity.UUID.Typed ()
import Data.UUID.Typed

spec :: Spec
spec = do
    eqSpec @(UUID Int)
    genValidSpec @(UUID Int)
    jsonSpec @(UUID Int)
