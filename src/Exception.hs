module Exception where

import Lib
import Data.Either

data Except e m a = Throw' e
                  | forall x. Catch' (m x) ( e -> m x ) (x -> m a)

instance Functor m => Functor (Except e m) where
  fmap f (Throw' a) = Throw' a
  fmap f (Catch' a b c) = Catch' a b (fmap f . c)

instance HFunctor (Except e) where
  hmap f (Throw' x) = Throw' x
  hmap f (Catch' p h k) = Catch'  (f p) (f . h) (f . k)

instance Syntax (Except e) where
  emap f (Throw' x) = Throw' x
  emap f (Catch' p h k) = Catch'  p h (f . k)
  handle c hdl (Throw' e) = Throw' e
  handle c hdl (Catch' p h k) =
    Catch' (hdl (fmap (const p) c))
           (\e -> hdl (fmap (const (h e)) c))
           (hdl . fmap k)

pattern Throw e <- (project -> Just (Throw' e))

throw :: (Member (Except e) s) => e -> Prog s a
throw e = send (Throw' e)

pattern Catch p h k <- (project -> Just (Catch' p h k))

catch :: (Member (Except e) s) => Prog s a -> (e -> Prog s a) -> Prog s a
catch p h = send (Catch' p h return)

runException :: Syntax sig =>
  Prog (Except e + sig) a -> Prog sig (Either e a)
runException (Return x) = return (Right x)
runException (Throw x) = return (Left x)
-- runException (Catch p h k) = do
--   r <- runException p
--   case r of
--       Left e -> do
--         r' <- runException (h e)
--         case r' of
--           Left e' -> return $ Left e'
--           Right a -> runException (k a)
--       Right x -> runException (k x)
-- runException (Other op) = Op $ handle (Right ()) hdl op
--   where
--     hdl = either (return . Left) runException
