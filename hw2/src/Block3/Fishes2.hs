{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block3.Fishes2 where
import           Data.Functor (Functor, fmap)
import           Prelude      (id)

-- Блок 3: Базовые классы типов
-- Задание 1: Рыбки, часть 2
class Monad m where
    return     :: a -> m a
    (>>=)      :: m a -> (a -> m b) -> m b

    {-: LAWS
        1. m >>= return    ≡ m
        2. return a >>= f  ≡ f a
        3. (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
    -}

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

    {-: LAWS
        1. f >=> returnFish ≡ f
        2. returnFish >=> f ≡ f
        3. (f >=> g) >=> h  ≡ f >=> (g >=> h)
    -}

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

    {-: LAWS
        1. join . pure            ≡ id
        2. join . fmap returnJoin ≡ id
        3. join . fmap join       ≡ join . join
    -}

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join       = id >=> id

-- 1. join . pure            ≡ id
-- join . pure ≡ \x -> join (pure x)
--             ≡ \x -> (id >=> id) (pure x)
--             = \x -> ((id >=> id) . pure) x = ((>=> id) . id . pure) x
--             ≡ \x -> (\y -> id y >>= id) (return x)
--             ≡ \x -> (id (return x) >>= id)
--             ≡ \x -> (return x >>= id)
--             ≡ \x -> (id x)
--             ≡ \x -> x
--             ≡ id

instance (Functor m, MonadJoin m) => Monad m where
    return  = returnJoin
    m >>= f = join (fmap f m)

-- m >>= return ≡ m
-- m >>= return ≡ m >>= returnJoin
--              ≡ join (fmap returnJoin m)
--              ≡ (join . fmap returnJoin) m
--              ≡ id m
--              ≡ m

instance (Functor m, MonadJoin m) => MonadFish m where
    returnFish = returnJoin
    (f >=> g) x = join (fmap g (f x))

-- 1. f >=> returnFish ≡ f
-- f >=> returnFish ≡ f >=> returnJoin
--                  ≡ \x -> join (fmap returnJoin (f x))
--                  ≡ \x -> (join . fmap returnJoin) (f x)
--                  ≡ \x -> id (f x)
--                  ≡ \x -> f x
--                  ≡ f
