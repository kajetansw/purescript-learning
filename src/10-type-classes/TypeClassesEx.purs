module TypeClassesEx where
  
import Prelude

import Data.Array ((:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)


-- chapter 6.4

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex c) = show c.real <> show c.imaginary

-- chapter 6.7

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = eq x y && eq xs ys

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (append xs $ append [y] ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) $ map f xs

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = false
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite x) (Finite y) = eq x y

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare _ Infinite = LT
  compare Infinite _ = GT
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f b (NonEmpty x xs) = foldl f b (x : xs)
  foldr f b (NonEmpty x xs) = foldr f b (x : xs)
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
  foldr f b (OneMore x ys) = f x (foldr f b ys)
  foldl f b (OneMore x ys) = f (foldl f b ys) x
  foldMap f (OneMore x ys) = append (f x) (foldMap f ys)