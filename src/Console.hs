module Console where

import Lib
import Control.Monad
import System.IO

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

runConsole :: Prog (Console + HVoid) a -> Prog HVoid (IO a)
runConsole (Return a) = return (return a)
-- runConsole (Print s a) = runConsole a
runConsole (Print s a) = do return $! do !x <- putStrLn "Something"; hFlush stdout
                            runConsole a
runConsole (GetLine k) = do
  return $ do s <- getLine; (runIO . removeVoid . k) s

runConsole (Other op) = Op $ handle (return ()) f op
  where
    f :: Syntax sig =>
      forall x. IO (Prog (Console + sig) x) -> Prog sig (IO x)
    f ma = undefined

removeVoid :: Syntax f => Prog (f + HVoid) a ->  Prog f a
removeVoid (Return x) = Return x
removeVoid (Op (Inl op)) = Op $ hmap removeVoid op


runIO :: Prog Console a -> IO a
runIO (Return x) = return x
runIO (Print s a) = do putStrLn s; runIO a
runIO (GetLine k) = do
  s <- getLine
  runIO (k s)
