module Nondet where

import Lib

data Nondet m a = Nondet' [m a]
                    deriving Functor

instance HFunctor Nondet where
  hmap h (Nondet' mas) = Nondet' $ fmap h mas

instance Syntax Nondet where
  emap f (Nondet' mas) = Nondet' $ fmap f mas
  handle c hdl (Nondet' mas) = Nondet' [ hdl $ fmap (const ma) c | ma <- mas ]

nondet :: (Member Nondet s) => [a] -> Prog s a
nondet as = send $ Nondet' $ fmap (return) as

pattern Nondet k <- (project -> Just (Nondet' k))

runNondetAll :: Syntax sig => Prog (Nondet + sig) a -> Prog sig [a]
runNondetAll (Return x) = return [x]
runNondetAll (Nondet mas) = fmap concat (traverse runNondetAll mas)
runNondetAll (Other op) = Op (handle [()] (fmap concat . traverse runNondetAll) op)
