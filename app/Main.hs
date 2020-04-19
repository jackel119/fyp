module Main where

import Control.Monad.Free
import Data.Maybe

import Async
import Lib
import Exception
import Console
-- import Tell
-- import Syntax
import State

main :: IO ()
main = return ()


prog0 :: Prog (Console + HVoid) ()
prog0 = do
  printLn "Hello"
  return ()

prog :: Prog (Console + (Thread + HVoid)) Int
prog = do
  printLn "Hello"
  fork (return ())
  return 10

prog2 :: Prog (State Int + HVoid) Int
prog2 = do
  x <- get
  put (x + 5)
  return $ x + 10

prog3 :: (Member Console s, Member Thread s, Member (State Int) s) => Prog s Int
-- prog3 :: Prog (Thread + ((State Int) + Console))Int
prog3 = do
  fork t1
  fork t3
  return 0
   where
     t1 = fork t2
     t2 = return ()
     t3 = return 15
