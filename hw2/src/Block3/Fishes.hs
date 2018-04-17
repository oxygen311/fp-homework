{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block3.Fishes where
import           Prelude (id)

-- Блок 3: Базовые классы типов
-- Задание 1: Рыбки
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

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \x -> f x >>= g

-- 1. f >=> returnFish ≡ f
-- f >=> returnFish ≡ \x -> f x >>= returnFish
--                  ≡ \x -> f x >>= return
--                  ≡ \x -> f x
--                  ≡ f

instance Monad m => MonadJoin m where
    returnJoin = return
    join m     = m >>= id

-- 1. join . pure ≡ id
-- join . pure ≡ \x -> join (pure x)
--             ≡ \x -> (pure x >>= id)
--             ≡ \x -> (return x >>= id)
--             ≡ \x -> id x
--             ≡ id

instance MonadFish m => Monad m where
    return  = returnFish
    m >>= f = (id >=> f) m

-- 1. m >>= return ≡ m
-- m >>= return ≡ m >>= returnFish
--              ≡ (id >=> returnFish) m
--              ≡ id m
--              ≡ m
