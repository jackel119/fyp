module A where

import Lib
import Control.Monad
import System.IO

data Dummy a = Dummy' a
             deriving (Functor, Show)

type A = Lift Dummy

-- deriving instance (Show a) => Show (Lift Dummy a)

pattern A k <- (project -> Just (Lift (Dummy' k)))

-- runA :: Syntax f => Prog ((Lift Dummy) + f) a -> IO (Prog f a)
-- runA (A x) = do putStrLn "A"
--                 runA x
-- runA (Other op) = return $ (fmap unId) $ Op $ handle (return ()) f op
--   where
--     f x =

doA :: (Member A s) => Prog s ()
doA = send $ Lift (Dummy' (return ()))

runA :: Syntax f => Prog (A + f) a -> IO (Prog f a)
runA (A x) = do putStrLn "A"
                runA x
runA (Return x) = return $ Return x
-- runA (Other op) = return $ fmap unId $ Op $ handle (Identity ()) f op
--   where
--     f :: Identity (Prog )

-- runIO :: Prog Console a -> IO a
-- runIO (Return x) = return x
-- runIO (Print s a) = do putStrLn s; runIO a
-- runIO (GetLine k) = do
--   s <- getLine
--   runIO (k s)
