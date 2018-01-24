{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RealmSpec where

import Data.Bifunctor
import Data.Realm
import Data.Monoid
import Data.MSet
import Numeric.Natural
import Test.QuickCheck

realmLaws :: forall r. (Arbitrary r, Show r, Realm r) => Property
realmLaws = forAll (arbitrary @(r,r,r)) $ \(k, m, n) ->
   foldl1 (.&&.) $ fmap (uncurry label . second property)
     [ ("commutative"  , commutativeLaw m n)
     , ("associative"  , associativeLaw k m n)
     , ("distributive" , distributiveLaw k m n)
     , ("identity"     , identityLaw k)
     , ("absorption"   , absorptionLaw m n)
     , ("idempotent"   , idempotentLaw k)
     , ("summation"    , summationLaw m n)
     ]

instance Arbitrary Natural where
  arbitrary = fmap (fromIntegral . abs) (arbitrary @Integer)

prop_natural_realm :: Property
prop_natural_realm = realmLaws @(Sum Natural)

prop_trivial_realm :: Property
prop_trivial_realm = realmLaws @()

prop_product_realm :: Property
prop_product_realm = realmLaws @(Sum Natural, Sum Natural)

prop_gcd_lcm_realm :: Property
prop_gcd_lcm_realm = realmLaws @(Product Natural)

prop_any_realm :: Property
prop_any_realm = realmLaws @Any

prop_maybe_realm :: Property
prop_maybe_realm = realmLaws @(Maybe (Sum Natural))

return []
tests :: IO Bool
tests = $quickCheckAll
