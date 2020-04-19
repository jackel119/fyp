module Syntax where

import Lib
import Control.Monad.Free

-- data Types = TNum | TBool | TUnit

-- data ENum  = ENum Int | Plus ENum ENum
--               deriving (Show, Eq)

-- data EBool = EBool Bool | Not EBool | And EBool EBool
--            deriving (Show, Eq)

-- -- a is Type
-- data EVar a = EVar VarName

-- data EUnit = EUnit

-- type VarName = String

-- -- Syntax Constructs as Signatures / Functors
-- data IfElse a where
--   IfElse :: EBool -> a -> a -> IfElse a
--   deriving (Show, Functor)

-- ifelse :: (Member IfElse s) => EBool -> a -> a -> Free s a
-- ifelse b x1 x2 = send $ IfElse b (Pure x1) (Pure x2)

-- data Func a where
--   Fn :: VarName -> a -> Func a
--   deriving (Show, Functor)

-- func :: (Member Func s) => VarName -> a -> Free s a
-- func name x = send $ Fn name $ Pure x

-- -- data FnApp a where

-- data Let a b where
--   Let :: EVar a -> a -> b -> Let a b
--   deriving (Show, Functor)

-- let' :: (Member (Let a) s) => VarName -> a -> b -> Free s b
-- let' name a b = send $ Let name a (Pure b)

-- class OfType a b -- where
--   -- eval :: a -> b

-- instance OfType ENum ENum
-- instance OfType EBool EBool
-- instance OfType EUnit EUnit

-- instance OfType (EVar a) a
-- instance OfType a b => OfType (IfElse a) b
-- instance OfType a b => OfType (Func a) b
-- instance OfType a b => OfType (Let x a) b
-- instance OfType a b => OfType (Free s a) b
