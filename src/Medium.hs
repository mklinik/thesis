{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Simple where

import Data.Functor

-- functor for natural numbers: FX = 1+X
data NatF a = Zero | Succ a

-- functor for natural numbers with computation steps: FX = 1+X+X, defined in terms of NatF
data NatCompF a = N (NatF a) | Step a

instance Functor NatF where
  fmap f Zero = Zero
  fmap f (Succ a) = Succ (f a)

-- type level fixed point combinator
data Fix f = In (f (Fix f))
out :: Fix f -> f (Fix f)
out (In x) = x

-- Nat is the fixed point of the functor NatF
type Nat = Fix NatF
-- NatComp is the fixed point of the functor NatCompF
type NatComp = Fix NatCompF

instance Show a => Show (NatF a) where
  show Zero     = "0"
  show (Succ n) = "S" ++ show n

instance Show a => Show (NatCompF a) where
  show (N n)    = show n
  show (Step n) = "_" ++ show n

instance Show Nat where
  show = show . out

instance Show NatComp where
  show = show . out

-- embeds Nat into NatComp
embed :: Nat -> NatComp
embed (In Zero) = In (N Zero)
embed (In (Succ n)) = In (N (Succ $ embed n))

-- projects NatComp onto Nat
project :: NatComp -> Nat
project (In (N n)) = In (fmap project n)
project (In (Step n)) = project n

-- the number two in Nat
two :: Nat
two = In $ Succ $ In $ Succ $ In Zero

-- the number two in NatComp, without any computation steps
twoC :: NatComp
twoC = In $ N $ Succ $ In $ N $ Succ $ In $ N Zero

-- the number two in NatComp, with some variations of computation steps
-- _SS0
twoC_1 :: NatComp
twoC_1 = In $ Step $ In $ N $ Succ $ In $ N $ Succ $ In $ N Zero

-- S_S_0
twoC_2 :: NatComp
twoC_2 = In $ N $ Succ $ In $ Step $ In $ N $ Succ $ In $ Step $ In $ N $ Zero

-- infinite successors
infsucc :: NatComp
infsucc = In (N (Succ infsucc))

-- infinite computation steps
infstep :: NatComp
infstep = In (Step infstep)
