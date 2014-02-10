{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module General where

import Data.Functor

data NatF a = Zero | Succ a

data G f a = F (f a) | Step a

instance Functor NatF where
  fmap f Zero = Zero
  fmap f (Succ a) = Succ (f a)

data Fix f = In (f (Fix f))
out :: Fix f -> f (Fix f)
out (In x) = x

embed :: Functor f => Fix f -> Fix (G f)
embed (In a) = In (F (fmap embed a))

project :: Functor f => Fix (G f) -> Fix f
project (In (Step a)) = project a
project (In (F a)) = In (fmap project a)

type Nat = Fix NatF
type NatComp = Fix (G NatF)

instance Show a => Show (NatF a) where
  show Zero     = "0"
  show (Succ n) = "S" ++ show n

instance Show (Fix NatF) where
  show = show . out

infstep :: NatComp
infstep = In (Step infstep)

infsucc :: NatComp
infsucc = In (F (Succ infsucc))
