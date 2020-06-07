module Thread where

import Data.Coerce
import Unsafe.Coerce
import Lib
import Tell
import State
import Control.Monad

import qualified Data.Map as Map

data Thread m a = Yield' (m a)
                | forall x. Fork' (Promise x) (m x) (m a)
                | forall x. Await' (Promise x) (x -> m a)

data Promise x = TID ThreadId

instance Functor m => Functor (Thread m) where
  fmap f (Yield' ma) = Yield' $ fmap f ma
  fmap f (Fork' h mx ma) = Fork' h mx $ fmap f ma
  fmap f (Await' p k) = Await' p $ (fmap f . k)

instance HFunctor Thread where
  hmap f (Yield' ma) = Yield' $ f ma
  hmap f (Fork' h mx ma) = Fork' h (f mx) (f ma)
  hmap f (Await' p k) = Await' p $ (f . k)

instance Syntax Thread where
  emap f (Yield' ma) = Yield' $ f ma
  emap f (Fork' h mx ma) = Fork' h mx $ f ma
  emap f (Await' p k) = Await' p $ (f . k)
  handle c hdl (Yield' p) = Yield' $ hdl (fmap (const p) c)
  handle c hdl (Fork' (TID x) d p) = Fork' (TID x)
                                     (hdl (fmap (const d) c))
                                     (hdl (fmap (const p) c))
  handle c hdl (Await' p k) = Await' p $ (\x -> hdl $ fmap (const x) c) . k

pattern Yield p <- (project -> Just (Yield' p))
pattern Fork h p q <- (project -> Just (Fork' h p q))
pattern Await p q <- (project -> Just (Await' p q))

yield :: Threaded s => Prog s ()
yield = send (Yield' (return ()))

fork :: Threaded s => Prog s a -> Prog s (Promise a)
fork q =  do
          id :: ThreadId <- get
          put (id + 1)
          send (Fork' (TID id) q (return $ TID id))

await :: Threaded s => Promise a -> Prog s a
await p = send $ Await' p return

data Daemon sig = forall x. Daemon (Prog (Thread + sig) x) ThreadId

type Threaded s = (Member Thread s, Member (State ThreadId) s)

type ThreadId = Int

data SThread sig r = SYield (Prog (Thread + sig) r)
                   | SFork (Daemon sig) (Prog (Thread + sig) r)
                   | forall x.
                      SAwait (Promise x) (x -> Prog (Thread + sig) r)
                   | SActive r

instance Syntax sig => Functor (SThread sig) where
  fmap f (SYield op) = SYield $ liftM f op
  fmap f (SFork p q) = SFork p $ liftM f q
  fmap f (SAwait p q) = SAwait p $ liftM (liftM f) q
  fmap f (SActive r) = SActive $ f r

runThread :: Syntax sig =>
  Prog (Thread + sig) r -> Prog sig (SThread sig r)
runThread (Return x) = return (SActive x)
runThread (Yield op) = return (SYield op)
runThread (Fork (TID tid) p q) = return $ SFork (Daemon p tid) q
runThread (Await p k) = return $ SAwait p k
runThread (Other op) = Op (handle (SActive ()) thread op)
  where
    thread :: Syntax sig => forall x. SThread sig (Prog (Thread + sig) x) -> Prog sig (SThread sig x)
    thread (SActive p) = runThread p
    thread (SYield p) = return (SYield (join p))
    thread (SAwait p k) = return (SAwait p (join . k))
    thread (SFork p q) = return (SFork p (join q))

data Finished = forall x. Finished x

schedule :: forall a sig. Syntax sig =>
  Prog (State ThreadId + (Thread + sig)) a -> Prog sig a
schedule p = master (fmap snd $ runState 0 p) [] Map.empty where
              -- p is master thread,
              -- ds is unfinished threads
              -- fs is finished thread results
              master :: Prog (Thread + sig) b -> [Daemon sig]
                          -> Map.Map ThreadId Finished
                          -> Prog sig b
              master p ds fs = do
                r <- runThread p
                case r of
                     SActive x -> case ds of
                                       [] -> return x
                                       _ -> daemons ds [] fs p
                     SAwait (TID tid) k -> case isFinished tid fs of
                                Just x -> master (k x) ds fs
                                Nothing -> daemons ds [] fs p
                     SYield p -> daemons ds [] fs p
                     SFork d p -> daemons (d : ds) [] fs p
              daemons :: [Daemon sig] -> [Daemon sig]
                          -> Map.Map ThreadId Finished
                          -> Prog (Thread + sig) b -> Prog sig b
              -- daemons activeThreads
              --         sleepingThreads
              --         finishedThreads
              --         masterThread
              daemons [] ds' fs p = master p (reverse ds') fs
              daemons (current@(Daemon q tid) : ds) ds' fs p = do
                r <- runThread q
                case r of
                     SActive x -> daemons ds ds'
                                    (Map.insert tid (Finished x) fs) p
                     SYield a -> daemons ds (Daemon a tid : ds') fs p
                     SFork a b -> daemons (a : ds)  (Daemon b tid : ds') fs  p
                     SAwait (TID tid) k -> case isFinished tid fs of
                                Just x  -> daemons (Daemon (k x) tid : ds) ds' fs p
                                Nothing -> daemons ds (current:ds') fs p
              isFinished :: ThreadId ->
                Map.Map ThreadId Finished -> Maybe r
              isFinished tid fs =
                case Map.lookup tid fs of
                          Just (Finished x) -> Just (unsafeCoerce x)
                          Nothing -> Nothing



type ThreadLog = [String]

-- runThreadWithLog :: Syntax sig =>
--   Prog (Thread + sig) r -> ThreadId -> Prog sig (SThread sig r)
-- runThreadWithLog (Return x) c = return (SActive x)
-- runThreadWithLog (Yield op) c = return (SYield op)
-- runThreadWithLog (Fork h p q) c = return $ SFork (Daemon p (c + 1)) q
-- runThreadWithLog (Other op) c = Op (handle (SActive ()) thread op)
--   where
--     thread :: Syntax sig => forall x. SThread sig (Prog (Thread + sig) x) -> Prog sig (SThread sig x)
--     thread (SActive p) = runThreadWithLog p c
--     thread (SYield p) = return (SYield (join p))
--     thread (SFork p q) = return (SFork p (join q))

-- scheduleWithLog :: Syntax sig => Prog (Thread + sig) a -> Prog sig (ThreadLog, a)
-- scheduleWithLog p = master p [] 0 [] where
--               master p ds count logAcc = do
--                 r <- runThreadWithLog p count
--                 case r of
--                      SActive x -> let msg = "MThread 0 Finished"
--                                       in return (reverse (msg:logAcc), x)
--                      SYield p -> let msg = "MThread 0 Yielded"
--                                       in daemons ds [] count (msg:logAcc) p
--                      SFork d@(Daemon m no) p -> do
--                        let msg = "MThread 0 Forked Off Thread " ++ (show no)
--                        daemons (d : ds) [] (count + 1) (msg:logAcc) p
--               daemons [] ds' count logAcc p = master p (reverse ds') count logAcc
--               daemons (Daemon q currentId : ds) ds' count logAcc p = do
--                 r <- runThreadWithLog q count
--                 case r of
--                      SActive _ -> do
--                        let msg = "Thread " ++ (show currentId) ++ " Finished"
--                        daemons ds ds' count (msg:logAcc) p
--                      SYield a -> do
--                        let msg = "Thread " ++ (show currentId) ++ " Yielded"
--                        daemons ds (Daemon a currentId : ds') count (msg:logAcc) p
--                      SFork a b -> do
--                        let newCount = count + 1
--                            msg = "Thread " ++ (show currentId) ++ " Forked off Thread" ++ (show newCount)
--                        daemons (a : ds)  (Daemon b count : ds') newCount (msg:logAcc) p
