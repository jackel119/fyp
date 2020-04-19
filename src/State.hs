module State where

import Lib

data State s m a = Get' (s -> m a)
                 | Put' s (m a)

instance Functor m => Functor (State s m) where
  fmap f (Get' k) = Get' $ fmap f . k
  fmap f (Put' s ma) = Put' s (fmap f ma)

instance HFunctor (State s) where
  hmap f (Get' k) = Get' $ f . k
  hmap f (Put' s ma) = Put' s (f ma)

instance Syntax (State s) where
  emap f (Get' k) = Get' $ f . k
  emap f (Put' s ma) = Put' s (f ma)
  handle c hdl (Get' k) = Get' $ hdl . (\x -> fmap (const x) c) . k

pattern Get k <- (project -> Just (Get' k))

get :: (Member (State s) r) => Prog r s
get = send (Get' return)

pattern Put s k <- (project -> Just (Put' s k))

put :: (Member (State s) r) => s -> Prog r ()
put s = send (Put' s (return ()))

runState :: Syntax sig => s -> Prog (State s + sig) a -> Prog sig (s, a)
runState s (Return a) = return (s, a)
runState s (Get k) = runState s (k s)
runState s (Put s' k) = runState s' k
runState s (Other op) = Op $ handle (s, ()) (uncurry runState) op
