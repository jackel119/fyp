module Lib where

import Control.Monad.Free

-- Define Functor Coproduct
data (f1 + f2) (m :: * -> *) a = Inl (f1 m a) | Inr (f2 m a)

-- Sig :: (* -> *) -> * -> *
data Prog sig a = Return a
                | Op (sig (Prog sig) a)

instance Functor (Prog sig) where
  fmap f (Return a) = Return $ f a

instance Applicative (Prog sig) where
  pure = Return
  (<*>) = undefined

class HFunctor h where
  hmap :: (Functor f, Functor g) => (forall x. f x -> g x) -> (h f x-> h g x)

class HFunctor sig => Syntax sig where
  emap :: (Monad m) => (m a -> m b) -> (sig m a -> sig m b)
  handle :: (Monad m, Monad n, Functor c) =>
    c () -> (forall x. c (m x) -> n (c x)) -> (sig m a -> sig n (c a))

instance Syntax sig => Monad (Prog sig) where
  return v = Return v
  Return v >>= f = let !v' = v in f $! v'
  Op op >>= f = let !op' = op in Op (emap (>>= f) op')

instance (HFunctor f1, HFunctor f2) => HFunctor (f1 + f2) where
  hmap f (Inl x) = Inl (hmap f x)
  hmap f (Inr x) = Inr (hmap f x)

instance (Syntax f1, Syntax f2) => Syntax (f1 + f2) where
  emap f (Inl x) = Inl (emap f x)
  emap f (Inr x) = Inr (emap f x)
  handle c hdl (Inl op) = Inl $ handle c hdl op
  handle c hdl (Inr op) = Inr $ handle c hdl op


foldF :: Functor sig => (a -> b) -> (sig b -> b) -> (Free sig a -> b)
foldF gen alg (Pure x) = gen x
foldF gen alg (Free op) = alg $ fmap (foldF gen alg) op

class ModularCarrier c where
  fwd :: Monad m => m (c m) -> c m

-- fwd :: (Functor f) => f (Free f a) -> Free f a
-- fwd op = Free op

finally :: Monad m => s -> m (s -> m a) -> m a
finally s = \p -> p >>= (\f -> f s)

data Void k
instance Functor Void where
  fmap = undefined
instance Show (Void k) where
  show = undefined

runVoid :: Free Void a -> a
runVoid (Pure x) = x

class (Syntax r, Syntax s) => Member r s where
  inj :: r m a -> s m a
  proj :: s m a -> Maybe (r m a)

-- Any functor can be injected to itself
instance (Syntax r) => Member r r where
  inj = id
  proj = Just

instance (Syntax r, Syntax b) => Member r (r + b) where
  inj = Inl
  proj (Inl a) = Just a
  proj (Inr _) = Nothing

instance (Syntax a, Member r b)
  => Member r (a + b) where
  inj = Inr . inj
  proj (Inr a) = proj a
  proj (Inl _) = Nothing

send :: (Member r s) => r (Prog s) a -> Prog s a
send = Op . inj

project :: (Member r s) => Prog s a -> Maybe (r (Prog s) a)
project (Op s) = proj s
project _ = Nothing

pattern Other s = Op (Inr s)

newtype (Lift sig) (m :: * -> *) a = Lift (sig (m a))

instance Functor sig => HFunctor (Lift sig) where
  hmap f (Lift x) = Lift $ fmap f x

instance Functor sig => Syntax (Lift sig) where
  emap f (Lift x) = Lift $ fmap f x
  handle c hdl (Lift op) = Lift $ fmap (\x -> hdl (fmap (const x) c)) op

type HVoid = Lift Void
run :: Prog HVoid a -> a
run (Return x) = x
