module Embed where

import Control.Monad
import Lib

newtype Embed m f a = Embed' (m (f a))

instance Functor sig => HFunctor (Embed sig) where
  hmap f (Embed' x) = Embed' $ fmap f x

instance (Monad sig) => Syntax (Embed sig) where
  emap f (Embed' x) = Embed' $ fmap f x
  handle c hdl (Embed' op) = Embed' $ fmap (\x -> hdl (fmap (const x) c)) op

-- pattern Embed k <- (project -> Just (Embed' k))

runEmbedded :: Monad m => Prog (Embed m) a -> m a
runEmbedded (Return x) = return x
runEmbedded (Op (Embed' mx)) = join $ fmap runEmbedded mx

embed :: (Monad m, Member (Embed m) r ) => m a -> Prog r a
embed mx = send $ Embed' (fmap Return mx)

run :: forall m a. Monad m => Prog (Embed m + HVoid) a -> m a
run = runEmbedded . removeVoid
  where
    removeVoid :: Prog (Embed m + HVoid) a -> Prog (Embed m) a
    removeVoid = translate f
    f (Inl a) = a
