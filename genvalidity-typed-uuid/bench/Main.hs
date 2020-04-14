{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.UUID.Typed ()
import Data.UUID.Typed
import Test.QuickCheck

main :: IO ()
main =
  Criterion.defaultMain
    [ genValidBench @(UUID Int),
      genUncheckedBench @(UUID Int),
      genBench "valid UUID via genUnchecked" ((genUnchecked `suchThat` isValid) :: Gen (UUID Int))
    ]
