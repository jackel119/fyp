module Console where

import Lib
import Embed
import Control.Monad
import Control.Monad.Identity
import System.IO
import Data.Coerce

data Console m a = Print' String (m a)
                 | GetLine' (String -> m a)

instance Functor m => Functor (Console m) where
  fmap f (Print' s ma) = Print' s (fmap f ma)
  fmap f (GetLine' k) = GetLine' $ (fmap f) . k

instance HFunctor (Console) where
  hmap h (Print' s ma) = Print' s (h ma)
  hmap h (GetLine' k) = GetLine' (h . k)

instance Syntax Console where
  emap f (Print' s ma) = Print' s (f ma)
  emap f (GetLine' k) = GetLine' (f . k)
  handle c hdl (Print' s ma) = Print' s $ hdl (fmap (const ma) c)
  handle c hdl (GetLine' k) = GetLine' $ hdl . (\x -> fmap (const x) c) . k

pattern Print s a <- (project -> Just (Print' s a))
pattern GetLine k <- (project -> Just (GetLine' k))

printLn :: Member Console s => String -> Prog s ()
printLn s = send $ Print' s (return ())

getLn :: Member Console s => Prog s ()
getLn = send $ GetLine' (\s -> return ())

toIO :: forall f s x. (Member f s, Syntax f, Member (Embed IO) s) =>
  Prog (Console + f) x -> Prog s x
toIO p = translate consoleToIO p

consoleToIO :: (Syntax f, Member f s, Member (Embed IO) s) =>
  (Console + f)  m a -> s m a
consoleToIO op = case proj op of
          Just (Print' s a) -> inj $ Embed' $ do putStrLn s; return a
          Just (GetLine' k) -> inj $ Embed' $ do x <- getLine
                                                 return $ k x
          Nothing -> let (Inr a) = op in inj a

runIO :: Prog Console a -> IO a
runIO (Return x) = return x
runIO (Print s a) = do putStrLn s; runIO a
runIO (GetLine k) = do
  s <- getLine
  runIO (k s)

-- runConsole :: Syntax sig => Prog (Console + sig) a -> Prog sig (IO a)
-- runConsole (Return x) = return $ return x
-- runConsole (Print s a) = fmap (\x -> putStrLn s >> x) $ runConsole a
-- runConsole (GetLine k) = ??
-- runConsole (Other op) = Op $ handle (return ()) runConsole op

greet :: Console (Console Identity) ()
greet = GetLine' $ \x -> (Print' x (Identity ()))
