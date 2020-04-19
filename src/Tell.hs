module Tell where

import Control.Monad.Free

import Lib

data Tell w m a = Tell' w (m a)

instance Functor m => Functor (Tell w m) where
  fmap f (Tell' w ma) = Tell' w (fmap f ma)

instance HFunctor (Tell w) where
  hmap h (Tell' w ma) = Tell' w (h ma)

instance Syntax (Tell w) where
  emap f (Tell' w ma) = Tell' w (f ma)
  handle c hdl (Tell' w ma) = Tell' w $ hdl (fmap (const ma) c)

tell :: (Member (Tell w) sig) => w -> Prog sig ()
tell x = send (Tell' x (return ()))

pattern Tell w a <- (project -> Just (Tell' w a))

runTell :: (Syntax sig) =>
  Prog (Tell w + sig) a -> Prog sig ([w], a)
runTell x = runTell' [] x
  where
    runTell' :: (Syntax sig) =>
      [w] -> Prog (Tell w + sig) a -> Prog sig ([w], a)
    runTell' acc (Return a) = return (acc, a)
    runTell' acc (Tell w a) = runTell' (acc ++ [w]) a
    runTell' acc (Other op) = Op $ handle (acc, ()) (uncurry runTell') op
