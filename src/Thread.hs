module Thread where

import Lib
import Tell
import Control.Monad

data Thread m a = Yield' (m a)
                | forall x. Fork' (m x) (m a)

instance Functor m => Functor (Thread m) where
  fmap f (Yield' ma) = Yield' $ fmap f ma
  fmap f (Fork' mx ma) = Fork' mx $ fmap f ma

instance HFunctor Thread where
  hmap f (Yield' ma) = Yield' $ f ma
  hmap f (Fork' mx ma) = Fork' (f mx) (f ma)

instance Syntax Thread where
  emap f (Yield' ma) = Yield' $ f ma
  emap f (Fork' mx ma) = Fork' mx $ f ma
  handle c hdl (Yield' p) = Yield' $ hdl (fmap (const p) c)
  handle c hdl (Fork' d p) = Fork' (hdl (fmap (const d) c)) (hdl (fmap (const p) c))

pattern Yield p <- (project -> Just (Yield' p))
pattern Fork p q <- (project -> Just (Fork' p q))

yield :: Member (Thread) s => Prog s ()
yield = send (Yield' (return ()))

fork :: Member (Thread) s => Prog s a -> Prog s ()
fork q = send (Fork' q (return ()))


data Daemon sig = forall x. Daemon (Prog (Thread + sig) x) ThreadId

type ThreadId = Int

data SThread sig r = SYield (Prog (Thread + sig) r)
                   | SFork (Daemon sig) (Prog (Thread + sig) r)
                   | SActive r

instance Syntax sig => Functor (SThread sig) where
  fmap f (SYield op) = SYield $ liftM f op
  fmap f (SFork p q) = SFork p $ liftM f q
  fmap f (SActive r) = SActive $ f r

runThread :: Syntax sig =>
  Prog (Thread + sig) r -> ThreadId -> Prog sig (SThread sig r)
runThread (Return x) c = return (SActive x)
runThread (Yield op) c = return (SYield op)
runThread (Fork p q) c = return $ SFork (Daemon p c) q
runThread (Other op) c = Op (handle (SActive ()) thread op)
  where
    thread :: Syntax sig => forall x. SThread sig (Prog (Thread + sig) x) -> Prog sig (SThread sig x)
    thread (SActive p) = runThread p c
    thread (SYield p) = return (SYield (join p))
    thread (SFork p q) = return (SFork p (join q))

schedule :: Syntax sig => Prog (Thread + sig) a -> Prog sig a
schedule p = master p [] 0 where
              master p ds count = do
                r <- runThread p count
                case r of
                     SActive x -> return x
                     SYield p -> daemons ds [] count p
                     SFork d p -> daemons (d : ds) [] (count + 1) p
              daemons [] ds' count p = master p (reverse ds') count
              daemons (Daemon q currentId : ds) ds' count p = do
                r <- runThread q count
                case r of
                     SActive _ -> daemons ds ds' count p
                     SYield a -> daemons ds (Daemon a currentId : ds') count p
                     SFork a b -> daemons (a : ds)  (Daemon b count : ds') (count + 1) p


type ThreadLog = [String]

runThreadWithLog :: Syntax sig =>
  Prog (Thread + sig) r -> ThreadId -> Prog sig (SThread sig r)
runThreadWithLog (Return x) c = return (SActive x)
runThreadWithLog (Yield op) c = return (SYield op)
runThreadWithLog (Fork p q) c = return $ SFork (Daemon p (c + 1)) q
runThreadWithLog (Other op) c = Op (handle (SActive ()) thread op)
  where
    thread :: Syntax sig => forall x. SThread sig (Prog (Thread + sig) x) -> Prog sig (SThread sig x)
    thread (SActive p) = runThreadWithLog p c
    thread (SYield p) = return (SYield (join p))
    thread (SFork p q) = return (SFork p (join q))

scheduleWithLog :: Syntax sig => Prog (Thread + sig) a -> Prog sig (ThreadLog, a)
scheduleWithLog p = master p [] 0 [] where
              master p ds count logAcc = do
                r <- runThreadWithLog p count
                case r of
                     SActive x -> let msg = "MThread 0 Finished"
                                      in return (reverse (msg:logAcc), x)
                     SYield p -> let msg = "MThread 0 Yielded"
                                      in daemons ds [] count (msg:logAcc) p
                     SFork d@(Daemon m no) p -> do
                       let msg = "MThread 0 Forked Off Thread " ++ (show no)
                       daemons (d : ds) [] (count + 1) (msg:logAcc) p
              daemons [] ds' count logAcc p = master p (reverse ds') count logAcc
              daemons (Daemon q currentId : ds) ds' count logAcc p = do
                r <- runThreadWithLog q count
                case r of
                     SActive _ -> do
                       let msg = "Thread " ++ (show currentId) ++ " Finished"
                       daemons ds ds' count (msg:logAcc) p
                     SYield a -> do
                       let msg = "Thread " ++ (show currentId) ++ " Yielded"
                       daemons ds (Daemon a currentId : ds') count (msg:logAcc) p
                     SFork a b -> do
                       let newCount = count + 1
                           msg = "Thread " ++ (show currentId) ++ " Forked off Thread" ++ (show newCount)
                       daemons (a : ds)  (Daemon b count : ds') newCount (msg:logAcc) p
