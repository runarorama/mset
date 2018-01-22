module Data.MSet where

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MSet
-- Copyright   :  (c) 2018 Rúnar Bjarnason
-- License     :  BSD-style
-- Maintainer  :  runar@higher-order.com
-- Stability   :  provisional
-- Portability :  portable
--
-- An implementation of rational multisets based on "A new look at multisets"
-- by Norman J Wildberger.
--
-- Whereas in a regular @Set@ a given element occurs either zero times or once,
-- in an @MSet@ an element can have multiple occurrences, fractional
-- occurrences, or even negative occurrences.
--
-- Since many function names clash with @Prelude@ names, this module is usually
-- imported @qualified@, e.g.
--
-- >  import Data.MSet (MSet)
-- >  import qualified Data.MSet as MSet
-----------------------------------------------------------------------------

import Prelude hiding (foldMap)
import qualified Prelude as P
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Data.Realm
import Numeric.Natural

-- | An @MSet m a@ is a linear combination of objects in @a@ with coefficients
--   in @m@.
newtype MSet m a = MSet { unMS :: Map a m }

type Multiset a = MSet Natural a

type RatMSet a = MSet Rational a

type IntMSet a = MSet Int a

-- | Two multisets @a@ and @b@ are equal, if for any object @x@,
--   @multiplicity x a == multiplicity x b@
instance (Eq a, Eq m, Monoid m) => Eq (MSet m a) where
  x == y = occurList x == occurList y

-- | An @MSet@ @a@ is less than or equal to an @MSet@ @b@ if, for any object
--   @x@, @multiplicity a x ≤ multiplicity b x@
instance (Ord a, Ord m, Monoid m) => Ord (MSet m a) where
  x <= y = getAll $ foldMap ((All .) . (<=) . multiplicity x) y

instance (Show a, Show m, Eq m, Monoid m) => Show (MSet m a) where
  show m = "fromOccurList " ++ show (occurList m)

-- | Get the occurrence list of a given @MSet@
occurList :: (Eq m, Num m) => MSet m a -> [(a, m)]
occurList = filter ((/= 0) . snd) . occurList'

occurList' :: Monoid m => MSet m a -> [(a, m)]
occurList' = Map.toList . unMS

-- | Construct an @MSet@ from an occurrence list
fromOccurList :: (Ord a, Monoid m) => [(a, m)] -> MSet m a
fromOccurList = MSet . Map.fromListWith mappend

-- | Get the multiplicity of an object in a given @MSet@.
multiplicity :: (Ord a, Monoid m) => MSet m a -> a -> m
multiplicity (MSet m) a = Map.findWithDefault mempty a m

-- | An object @x@ occurs in @s@ if the multiplicity of @x@ is nonzero
--   or, equivalently, if its multiplicity is @1@ in any scalar multiple of
--   @s@.
occursIn :: (Ord a, Eq m, Monoid m) => a -> MSet m a -> Bool
occursIn a m = multiplicity m a /= mempty

-- | @a isFrom b@ when every element of @a@ is also in @b@.
isFrom :: (Ord a, Eq m, Monoid m) => MSet m a -> MSet m a -> Bool
isFrom m = getAll . foldMap (\a _ -> All $ a `occursIn` m)

-- | Fold an @MSet@ with a monoid
foldMap :: (Eq m, Monoid m, Monoid b) => (a -> m -> b) -> MSet m a -> b
foldMap f = P.foldMap (uncurry f) . occurList

foldMap' :: (Monoid m, Monoid b) => (a -> m -> b) -> MSet m a -> b
foldMap' f = P.foldMap (uncurry f) . occurList'

-- | The size of an @MSet@ is the sum of its multiplicities.
size :: Monoid m => MSet m a -> m
size = foldMap' (const id)

-- | The empty @MSet@
empty :: MSet m a
empty = MSet Map.empty

-- | Insert one occurrence of an object into an @MSet@
insert :: Num m => a -> MSet m a -> MSet m a
insert a m = m <> fromList [a]

fromList :: Num m => [a] -> MSet m a
fromList = fromOccurList . map (,1)

instance (Ord a, Realm m) => Realm (MSet m a) where
  MSet m \/ MSet n = MSet $ Map.unionWith (\/) m n
  MSet m /\ MSet n = MSet $ Map.intersectionWith (/\) m n

instance (Ord a, Monoid m) => Monoid (MSet m a) where
  MSet m `mappend` MSet n = MSet $ Map.unionWith mappend m n
  mempty = empty

-- | Modify the occurrences of an @MSet@ by a function
mapOccurs :: (m -> n) -> MSet m a -> MSet n a
mapOccurs f (MSet m) = MSet $ Map.map f m

-- | Scale an @MSet@ by a number, if that @MSet@ has numeric coefficients
scale :: Num n => n -> MSet n a -> MSet n a
scale n m = mapOccurs (n *) m

-- | Scale an @MSet@ by a natural number, if that @MSet@ has non-numeric
--   coefficients
scale' :: (Ord a, Monoid m) => Natural -> MSet m a -> MSet m a
scale' n a | n == 0 = empty
           | otherwise = a <> scale' (pred n) a

-- | The direct product of two @MSet@s. The multiplicity of `(a,b)` will be
--   the product of the multiplicity of `a` and the multiplicity of `b`.
product :: Num m => MSet m a -> MSet m b -> MSet m (a,b)
product m n = (,) <$> m <*> n

instance (Ord a, Monoid m) => Functor (MSet m a) where
  fmap f m = fromOccurList $ map (first f) occurList m

instance (Ord a, Num m, Monoid m) => Applicative (MSet m a) where
  pure = fromList pure
  f <*> m = fromOccurList $
    (\(a,ma) (b,mb) -> (f a b, ma * mb)) <$> occurList m <*> occurList n

instance (Ord a, Group m) => Group (MSet a) where
  neg m = mapOccurs neg


