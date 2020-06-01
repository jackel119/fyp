module Main where

import Control.Monad.Free
import Data.Maybe

import AsyncAwait
import Lib
import Exception
import Console
import Embed
import Nondet
import State

main :: IO ()
main = return ()

-- progE :: Members '[Embed IO, Console] r => Prog r ()
-- progE = do
--   embed $ putStrLn "Hello"
--   embed $ putStrLn "World"
--   return ()

prog :: Prog (State Int + (Console + HVoid)) ()
prog = do
  x :: Int <- get
  printLn "Hello"
  put $ ((x :: Int) + 5 :: Int)
  return ()

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
