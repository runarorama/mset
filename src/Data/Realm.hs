{-# LANGUAGE FlexibleInstances #-}


module Data.Realm where

import Control.Applicative
import Data.Monoid
import Numeric.Natural

-- | A realm is a monoid and a distributive lattice, satisfying the following:
--
-- Commutative laws
--
-- @
-- m <> n ≡ n <> m
-- m \/ n ≡ n \/ m
-- m /\ n ≡ n /\ m
-- @
--
-- Associative laws
--
-- @
-- k <> (m <> n) ≡ (k <> m) <> n
-- k \/ (m \/ n) ≡ (k \/ m) \/ n
-- k /\ (m /\ n) ≡ (k /\ m) /\ n
-- @
--
-- Distributive laws
--
-- @
-- k <> (m \/ n) ≡ (k <> m) \/ (k <> n)
-- k <> (m /\ n) ≡ (k <> m) /\ (k <> n)
-- k /\ (m \/ n) ≡ (k /\ m) \/ (k /\ n)
-- k \/ (m /\ n) ≡ (k \/ m) /\ (k \/ n)
-- @
--
-- Identity laws
--
-- @
-- mempty <> m ≡ m
-- mempty \/ m ≡ m
-- mempty /\ m ≡ mempty
-- @
--
-- Absorption laws
--
-- @
-- m \/ (m /\ n) ≡ m
-- m /\ (m \/ n) ≡ m
-- @
--
-- Idempotent laws
--
-- @
-- m \/ m ≡ m
-- m /\ m ≡ m
-- @
--
-- Summation law
--
-- @
-- (m \/ n) <> (m /\ n) ≡ m <> n
-- @
--
-- Some realms may additionally obey a cancellation law, and we call
-- these cancellative realms:
--
-- @
-- (k <> n = m <> n) => (k = m)
-- @
--
-- @Realm@ is a subclass of @Ord@ because any join semilattice defines an order
--
-- @
-- m <= n = m \/ n == n
-- @
class (Ord a, Monoid a) => Realm a where
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a

-- | Natural numbers with addition form a realm
instance Realm (Sum Natural) where
  a \/ b = max a b
  a /\ b = min a b

-- | Booleans with disjunction form a realm
instance Realm Any where
  Any a \/ Any b = Any (a || b)
  Any a /\ Any b = Any (a && b)

-- | The trivial realm
instance Realm () where
  () \/ () = ()
  () /\ () = ()

-- | Realms are closed under products
instance (Realm a, Realm b) => Realm (a,b) where
  (a,b) \/ (c,d) = (a \/ c, b \/ d)
  (a,b) /\ (c,d) = (a /\ c, b /\ d)

-- | The integers under multiplication form a realm with @gcd@ and @lcm@ as
--   meet and join, respectively
instance Integral a => Realm (Product a) where
  Product m \/ Product n = Product (lcm n m)
  Product m /\ Product n = Product (gcd n m)

instance Realm a => Realm (Maybe a) where
  n \/ m = ((\/) <$> n <*> m) <|> n <|> m
  n /\ m = ((/\) <$> n <*> m)

commutativeLaw :: Realm r => r -> r -> Bool
commutativeLaw m n =
  and [ m <> n == n <> m
      , m \/ n == n \/ m
      , m /\ n == n /\ m
      ]

associativeLaw :: Realm r => r -> r -> r -> Bool
associativeLaw k m n =
  and [ k <> (m <> n) == (k <> m) <> n
      , k \/ (m \/ n) == (k \/ m) \/ n
      , k /\ (m /\ n) == (k /\ m) /\ n
      ]

distributiveLaw :: Realm r => r -> r -> r -> Bool
distributiveLaw k m n =
  and [ k <> (m \/ n) == (k <> m) \/ (k <> n)
      , k <> (m /\ n) == (k <> m) /\ (k <> n)
      , k /\ (m \/ n) == (k /\ m) \/ (k /\ n)
      , k \/ (m /\ n) == (k \/ m) /\ (k \/ n)
      ]

identityLaw :: Realm r => r -> Bool
identityLaw m =
  and [ mempty <> m == m
      , mempty \/ m == m
      , mempty /\ m == mempty
      ]

absorptionLaw :: Realm r => r -> r -> Bool
absorptionLaw m n =
  and [ m \/ (m /\ n) == m
      , m /\ (m \/ n) == m
      ]

idempotentLaw :: Realm r => r -> Bool
idempotentLaw m =
  and [ m /\ m == m
      , m \/ m == m
      ]

summationLaw :: Realm r => r -> r -> Bool
summationLaw m n =
  (m \/ n) <> (m /\ n) == m <> n

cancellationLaw :: Realm r => r -> r -> r -> Bool
cancellationLaw k m n =
  (k <> n /= m <> n) || (k == m)

