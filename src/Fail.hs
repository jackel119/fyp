module Fail where

import Lib
import Control.Monad.Free

data Fail k = Fail
            deriving Functor

-- fail :: (Member Fail s) => Free s a
-- fail = send Fail

-- interpretFail :: Functor s => Free (Fail + s) a -> Free s (Maybe a)
-- interpretFail (Free (Inl Fail)) = return Nothing
-- interpretFail (Free (Inr other)) = Free $ fmap interpretFail other
-- interpretFail (Pure x) = Pure $ Just x
