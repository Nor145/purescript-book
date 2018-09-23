module Data.Hashable
  ( HashCode
  , hashCode
  , class Hashable
  , hash
  , hashEqual
  , combineHashes
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

derive instance eqHashCode :: Eq HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashMaybe :: Hashable a => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = hashCode 1 `combineHashes` hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a `combineHashes` hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 `combineHashes` hash a
  hash (Right b) = hashCode 1 `combineHashes` hash b

-- 6.4

-- 1.
newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex x) = "(" <> show x.real <> " + " <> show x.imaginary <> "i)"

instance eqComplex :: Eq Complex where
  eq (Complex x) (Complex y) = x.real == y.real && x.imaginary == y.imaginary

-- 6.7

-- 1.
data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

-- 2.
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

-- 3.
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

-- 4.
data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = x == y
  eq _ _ = false

instance ordExtended ::  (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

-- 5.
instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x xs) = foldl f acc ([x] <> xs)
  foldr f acc (NonEmpty x xs) = foldr f acc ([x] <> xs)
  foldMap f (NonEmpty x xs) = foldMap f ([x] <> xs)

-- 6.
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore x xs) = f x (foldr f acc xs)
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldMap f (OneMore x xs) = append (f x) (foldMap f xs)

-- 6.11

class Monoid m <= Action m a where
    act :: m -> a -> a

-- 3
instance arrayAction :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

-- 4
newtype Self m = Self m

instance selfAppend :: Action m a => Action m (Self m) where
  act _ (Self x) = Self (x <> x)

-- 6.12

2.
hashDuplicates :: forall a. (Hashable a) => Array a -> Boolean
hashDuplicates a = length a /= length (nubBy hashEqual a)
