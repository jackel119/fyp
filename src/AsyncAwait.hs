module AsyncAwait where

import Control.Concurrent.Async
import Lib
import Tell
import Embed
import Control.Monad

                    -- forall b. Fork' (n b) (Async b -> m (c a))
data AsyncAwait m a = forall b. Fork' (m b) (Async b -> m a)
                        | forall b. Await' (Async b) (b -> m a)

  -- = forall b. Handle' (s -> m b) (b -> m a)
  --               | forall x. Fork' (m x) (m a)
  --               | Request' s (m a)

instance Functor m => Functor (AsyncAwait m) where
  fmap f (Fork' mb k) = Fork' mb (fmap f . k)
  fmap f (Await' p k) = Await' p ((fmap f) . k)

instance HFunctor AsyncAwait where
  hmap f (Fork' mb k) = Fork' (f mb) (f . k)
  hmap f (Await' p k) = Await' p (f . k)

instance Syntax AsyncAwait where
  emap f (Fork' mb k) = Fork' mb (f . k)
  emap f (Await' p k) = Await' p (f . k)
  -- handle c hdl (Fork' mb k) = undefined
  --   where
  --     f = hdl $ mb <$ c :: _
  --     g = (\x  :: Async b -> hdl $ fmap (const mb) c) :: _
  handle c hdl (Fork' mb k) = Fork' (hdl $ fmap (const mb) c)
                                    ((\x -> hdl $ fmap (const x) c)
                                      . k . (undefined)
                                    )
                                    -- (\x -> hdl $ fmap (const mb) c)
  handle c hdl (Await' p bma) = Await' p (\x -> hdl $ fmap (const (bma x)) c)
                  -- Await' (fmap (\mb -> hdl $ fmap (const mb) c) p)
                  --        (hdl . fmap bma)

pattern Fork p q <- (project -> Just (Fork' p q))
pattern Await p k <- (project -> Just (Await' p k))

fork :: (Member AsyncAwait r) => Prog r a -> Prog r (Async a)
fork p = send $ Fork' p return

await :: (Member AsyncAwait r) => Async a -> Prog r a
await p = send $ Await' p return

-- runAsync :: (Syntax sig, Members '[Embed IO, sig] r)
--           => Prog (AsyncAwait) a -> Prog r a
-- runAsync (Return x) = Return x
-- runAsync (Fork p q) = embed $ withAsync _
-- runAsync (Await p k) = _
