{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Main where

import           Control.Monad
import           Control.DeepSeq
import           Data.List
import           GHC.Generics (Generic, Generic1)
import           System.Exit
import           Test.Framework                       (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

import           Jq.Filters                            as Filter

{--
  It can be that one (or both) of these two derivation fail.
  Especially if you introduce some non-trivial constructors
  or if your definition of filter is mutually recursive with
  some other definition.
  This doesn't necessarily mean that you're doing anything wrong.
  You can try fixing it yourself by adding
  `deriving instance Generic X` and
  `deriving instance NFData X` below for the missing classes.
  In case this doesn't work reach out to the course team.
--}
deriving instance Generic Filter
deriving instance NFData Filter


instance Arbitrary Filter where
    arbitrary = do
        id  <- arbitrary :: Gen String
        f   <- arbitrary
        elements [ filterIdentitySC, filterIndexingSC id, filterPipeSC f f, filterCommaSC f f ]

main = defaultMain tests

prop_computes_identity      = total $ filterIdentitySC
prop_computes_indexing id   = total $ filterIndexingSC id
prop_computes_pipe f g      = total $ filterPipeSC f g
prop_computes_comma f g     = total $ filterCommaSC f g

prop_identity_refl          = filterIdentitySC  == filterIdentitySC
prop_indexing_refl  f g     = f == g ==>
                              filterIndexingSC f == filterIndexingSC f
prop_pipe_refl e f g h      = e == g && f == h ==>
                              filterPipeSC e f == filterPipeSC g h
prop_comma_refl e f g h     = e == g && f == h ==>
                              filterCommaSC e f == filterCommaSC g h

tests = [
    testGroup "Constructors are defined" [
        testProperty "Constructor for identity computes" prop_computes_identity
      , testProperty "Constructor for indexing computes" prop_computes_indexing
      , testProperty "Constructor for pipe computes" prop_computes_pipe
      , testProperty "Constructor for comma computes" prop_computes_comma]
  , testGroup "Reflection instances" [
        testProperty "Reflection identity" prop_identity_refl
      , testProperty "Reflection indexing" prop_indexing_refl
      , testProperty "Reflection pipe" prop_pipe_refl
      , testProperty "Reflection comma" prop_comma_refl]
    ]
