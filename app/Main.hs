module Main where

import Control.Monad.Free
import Data.Maybe

import qualified Control.Monad.State as S

import AsyncAwait
import Lib
import Thread
import Exception
import Console
import Embed
import Nondet
import State
import Fail

main :: IO ()
main = return ()

progE :: Members '[Embed IO, Console] r => Prog r ()
progE = do
  embed $ putStrLn "Hello"
  embed $ putStrLn "World"
  return ()

progWithTransformers :: S.StateT Int Maybe Int
progWithTransformers = do
  s <- S.get
  if s /= 0
     then do
        S.put 5
        return $ 100 `mod` s
     else S.lift Nothing

progWithEffects :: Prog (State Int + (Fail + HVoid)) Int
progWithEffects = do
  (s :: Int) <- get
  if s /= 0
     then do
        put (5 :: Int)
        return $ 100 `mod` s
     else Fail.fail

failToT ::
  (Syntax f, Member f r, Member (Embed (S.StateT Int Maybe)) r) =>
  (Fail + f) m a -> r m a
failToT (Inl Fail) = inj $ Embed' $ nothingT
    where
      nothingT :: S.StateT Int Maybe a
      nothingT = S.lift Nothing
failToT (Inr a) = inj a

stateToT :: forall f r m a.
  (Syntax f, Member f r, Member (Embed (S.StateT Int Maybe)) r) =>
  (State Int + f) m a -> r m a
stateToT (Inl (Get' k)) = inj $ Embed' $ gT
    where
      gT :: S.StateT Int Maybe (m a)
      gT = do
        (s :: Int) <- S.get
        return $ k s
stateToT (Inl (Put' s k)) = inj $ Embed' $ pT
    where
      pT :: S.StateT Int Maybe (m a)
      pT =  S.put s >> return k
stateToT (Inr a) = inj a

type ET = S.StateT Int Maybe

-- converted :: Prog (Fail + (Embed ET + HVoid)) Int
-- converted = translate stateToT progWithEffects

-- converted = (translate failToT . translate stateToT) progWithEffects

-- prog :: Prog (State Int + (Console + HVoid)) ()
-- prog = do
--   x :: Int <- get
--   printLn "Hello"
--   put $ ((x :: Int) + 5 :: Int)
--   return ()

progT :: (Member (Embed IO) r, Threaded r) => Prog r ()
progT = do
  greeted <- Thread.fork $ do
    yield
    embed $ putStrLn "hello world"
  embed $ putStrLn "hello to you too!"
  -- Thread.await greeted
  embed $ putStrLn "This comes last"

-- prog :: Prog (Console + (Thread + HVoid)) Int
-- prog = do
--   printLn "Hello"
--   fork (return ())
--   return 10

prog2 :: Prog (State Int + HVoid) Int
prog2 = do
  x <- get
  put (x + 5)
  return $ x + 10

-- prog3 :: (Member Console s, Member Thread s, Member (State Int) s) => Prog s Int
-- -- prog3 :: :qProg (Thread + ((State Int) + Console))Int
-- prog3 = do
--   fork t1
--   fork t3
--   return 0
--    where
--      t1 = fork t2
--      t2 = return ()
--      t3 = return 15
