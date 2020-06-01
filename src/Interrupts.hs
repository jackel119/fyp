module Async where

import Lib
import Tell
import Control.Monad

data Async s m a = forall b. Handle' (s -> m b) (b -> m a)
                | forall x. Spawn' (m x) (m a)
                | Request' s (m a)

instance Functor m => Functor (Async s m) where
  fmap f (Handle' smb bma) =  Handle' smb ((fmap f) . bma)
  fmap f (Spawn' mx ma) = Spawn' mx (fmap f ma)
  fmap f (Request' s ma) = Request' s (fmap f ma)

instance HFunctor (Async s) where
  hmap f (Handle' smb bma) =  Handle' (f . smb) (f . bma)
  hmap f (Spawn' mx ma) = Spawn' (f mx) (f ma)
  hmap f (Request' s ma) = Request' s (f ma)

instance Syntax (Async s) where
  emap f (Handle' smb bma) =  Handle' smb (f . bma)
  emap f (Spawn' mx ma) = Spawn' mx (f ma)
  emap f (Request' s ma) = Request' s (f ma)
  -- handle c hdl (Handle' smb bma) = Handle'
  handle c hdl (Request' s ma) = Request' s (hdl $ fmap (const ma) c)

-- instance Syntax Thread where
--   emap f (Yield' ma) = Yield' $ f ma
--   emap f (Fork' mx ma) = Fork' mx $ f ma
--   handle c hdl (Yield' p) = Yield' $ hdl (fmap (const p) c)
--   handle c hdl (Fork' d p) = Fork' (hdl (fmap (const d) c)) (hdl (fmap (const p) c))
