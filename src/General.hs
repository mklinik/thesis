{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

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
embed = In . F . fmap embed . out

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

instance (Functor f, Show a, Show (f a)) => Show (G f a) where
  show (F x)    =        show x
  show (Step x) = "_" ++ show x

instance Show (Fix (G NatF)) where
  show = show . out

-- infinite steps
infstep :: NatComp
infstep = In $ Step infstep

-- infinite successors
infsucc :: NatComp
infsucc = In $ F $ Succ infsucc

-- any number in Nat, given an Int
nat :: Int -> Nat
nat x = head $ reverse $ take (x+1) $ iterate (In . Succ) (In Zero)

-- the number two in NatComp, without any computation steps
twoC :: NatComp
twoC = In $ F $ Succ $ In $ F $ Succ $ In $ F Zero

-- the number two in NatComp, with some variations of computation steps
-- _SS0
twoC_1 :: NatComp
twoC_1 = In $ Step $ In $ F $ Succ $ In $ F $ Succ $ In $ F Zero

-- S_S_0
twoC_2 :: NatComp
twoC_2 = In $ F $ Succ $ In $ Step $ In $ F $ Succ $ In $ Step $ In $ F $ Zero
