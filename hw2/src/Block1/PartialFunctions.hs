{-# LANGUAGE TypeOperators #-}
module Block1.PartialFunctions
    ( partial, total, apply, applyOrElse, withDefault, isDefinedAt, orElse
    ) where

import           Control.Category (Category (..))
import           Control.Monad    (mplus, (<=<))
import           Data.Maybe       (fromMaybe, isJust)

-- Задание 2: Частичные функции
data a ~> b
    = Partial   (a -> Maybe b) -- a partial function
    | Defaulted (a ~> b) b     -- a partial function with a default value

partial :: (a -> Maybe b) -> a ~> b
partial = Partial

total   :: (a -> b) -> a ~> b
total f = Partial $ return Prelude.. f

apply                    :: (a ~> b) -> a -> Maybe b
apply (Partial f) x      = f x
apply (Defaulted f dv) x = return $ fromMaybe dv $ apply f x

applyOrElse        :: (a ~> b) -> a -> b -> b
applyOrElse f x dv = fromMaybe dv (apply f x)

withDefault                  :: (a ~> b) -> b -> (a ~> b)
withDefault (Defaulted f _) = Defaulted f
withDefault f               = Defaulted f

isDefinedAt :: (a ~> b) -> a -> Bool
isDefinedAt f v = isJust (apply f v)

orElse      :: (a ~> b) -> (a ~> b) -> a ~> b
orElse f g =  Partial $ \x -> apply f x `mplus` apply g x

instance Category (~>) where
    id = Partial return
    f . g = Partial $ apply f <=< apply g

-- Right Identity
-- apply (id . f) ≡ apply ((Partial return) . f)
--                ≡ apply (Partial $ apply (Partial return) <=< apply f)
--                ≡ apply (Partial return) <=< apply f
--                ≡ apply (Partial Just) <=< apply f
--                ≡ Just <=< apply f
--                ≡ apply f

-- Left Identity
-- apply (f . id) ≡ apply (f . (Partial return))
--                ≡ apply (Partial $ apply f <=< apply (Partial return))
--                ≡ apply f <=< apply (Partial return)
--                ≡ apply f <=< apply (Partial Just)
--                ≡ apply f <=< Just
--                ≡ apply f

-- Associativity
-- (f . g) . h ≡ Partial $ apply (f . g) <=< apply h
--             ≡ Partial $ apply (Partial $ apply f <=< apply g) <=< apply h
--             ≡ Partial $ (apply f <=< apply g) <=< apply h
--             ≡ Partial $ apply f <=< apply g <=< apply h

-- Associativity
-- f . (g . h) ≡ Partial $ apply f <=< apply (g . h)
--             ≡ Partial $ apply f <=< apply (Partial $ apply g <=< apply h)
--             ≡ Partial $ apply f <=< (apply g <=< apply h)
--             ≡ Partial $ apply f <=< apply g <=< apply h
