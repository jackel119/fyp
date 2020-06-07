module Fail where

import Lib
import Control.Monad.Free

data Fail (m :: * -> *) a = Fail

instance Functor m => Functor (Fail m) where
  fmap f Fail = Fail

instance HFunctor Fail where
  hmap f Fail = Fail

instance Syntax Fail where
  emap f Fail = Fail
  handle c hdl Fail = Fail

fail :: (Member Fail s) => Prog s a
fail = send Fail


-- interpretFail :: Functor s => Free (Fail + s) a -> Free s (Maybe a)
-- interpretFail (Free (Inl Fail)) = return Nothing
-- interpretFail (Free (Inr other)) = Free $ fmap interpretFail other
-- interpretFail (Pure x) = Pure $ Just x
