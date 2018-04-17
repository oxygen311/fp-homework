{-# LANGUAGE NoImplicitPrelude #-}

module Block3.ChaoticGood where
import           Control.Applicative (Applicative (..), pure, (<*>))
import           Data.Foldable       (Foldable (..), foldr)
import           Data.Functor        (Functor (..), fmap, (<$>))
import           Data.Monoid         (Monoid (..))
import           Data.Traversable    (Traversable (..), traverse)

-- Identity a
newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure               = Identity
    (Identity f) <*> a = fmap f a

instance Foldable Identity where
    foldr f ini (Identity a) = f a ini

instance Traversable Identity where
    traverse f (Identity a) = fmap Identity (f a)

-- Either a b
data Either e b = Left e | Right b

instance Functor (Either e) where
    fmap _ (Left e)  = Left e
    fmap f (Right x) = Right (f x)

instance Applicative (Either e) where
    pure = Right
    Left e  <*> _ = Left e
    Right f <*> x = fmap f x

instance Foldable (Either e) where
    foldr _ ini (Left _)  = ini
    foldr f ini (Right x) = f x ini

instance Traversable (Either e) where
    traverse f (Right x) = fmap Right (f x)
    traverse _ (Left e)  = pure (Left e)

-- Tree a
data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
    fmap _ Leaf         = Leaf

instance Foldable Tree where
    foldr _ z Leaf         = z
    foldr f z (Node x l r) = foldr f (f x (foldr f z r)) l

instance Applicative Tree where
    pure x                    = Node x (pure x) (pure x)
    Node f g h <*> Node x l r = Node (f x) (g <*> l) (h <*> r)
    _          <*> _          = Leaf

instance Traversable Tree where
    traverse f = foldr (\x ys -> (Node <$> f x) <*> pure Leaf <*> ys) (pure Leaf)
--     traverse _ Leaf = pure Leaf
--     traverse f (Node x l r) = (Node <$> f x) <*> traverse f l <*> traverse f r

-- Const a b
newtype Const a b = Const { getConst :: a }

instance Functor (Const m) where
    fmap _ (Const v) = Const v

instance Foldable (Const m) where
    foldMap _ _ = mempty

instance Monoid m => Applicative (Const m) where
    pure _                    = Const mempty
    (Const v1) <*> (Const v2) = Const (v1 `mappend` v2)

instance Traversable (Const m) where
    traverse _ (Const m) = pure (Const m)

-- Pair a b
data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap g (Pair x y) = Pair x (g y)

instance Foldable (Pair a) where
    foldr f ini (Pair _ y) = f y ini

instance Monoid a => Applicative (Pair a) where
  pure                      = Pair mempty
  (Pair u f) <*> (Pair v x) = Pair (u `mappend` v) (f x)

instance Traversable (Pair a) where
    traverse f (Pair x y) = fmap (Pair x) (f y)
