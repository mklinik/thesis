{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Simple where

import Data.Functor

-- functor for natural numbers: FX = 1+X
data NatF a = Zero | Succ a

-- functor for natural numbers with computation steps: FX = 1+X+X
data NatCompF a = ZeroC | SuccC a | Step a

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
  show (ZeroC)   = "0"
  show (SuccC n) = "S" ++ show n
  show (Step n)  = "_" ++ show n

instance Show Nat where
  show = show . out

instance Show NatComp where
  show = show . out

-- embeds Nat into NatComp
embed :: Nat -> NatComp
embed (In Zero)     = In ZeroC
embed (In (Succ n)) = In (SuccC $ embed n)

-- projects NatComp onto Nat
project :: NatComp -> Nat
project (In ZeroC)     = In Zero
project (In (SuccC n)) = In (Succ (project n))
project (In (Step n))  = project n


-- some numbers

-- the number two in Nat
two :: Nat
two = In $ Succ $ In $ Succ $ In Zero

-- the number two in NatComp, without any computation steps
twoC :: NatComp
twoC = In $ SuccC $ In $ SuccC $ In ZeroC

-- the number two in NatComp, with some variations of computation steps
-- _SS0
twoC_1 :: NatComp
twoC_1 = In $ Step $ In $ SuccC $ In $ SuccC $ In ZeroC

-- S_S_0
twoC_2 :: NatComp
twoC_2 = In $ SuccC $ In $ Step $ In $ SuccC $ In $ Step $ In $ ZeroC

-- infinite successors
infsucc :: NatComp
infsucc = In (SuccC infsucc)

-- infinite computation steps
infstep :: NatComp
infstep = In (Step infstep)
