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
project b = case out b of
  (Step b') -> project b'
  (F b')    -> In (fmap project b')


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



-- lists

data ListF a b = Nil | Cons a b

instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons a l) = Cons a (f l)

newtype List a = L (Fix (ListF a))
outL :: List a -> Fix (ListF a)
outL (L x) = x

-- expected: Fix (ListF b)
-- l ::      Fix (ListF a)
-- L l :: List a
-- fmap f (L l) :: List b
-- outList (fmap f (L l)) :: Fix (ListF b)

instance Functor List where
  fmap f (L (In Nil)) = L $ In Nil
  fmap f (L (In (Cons a l))) = L $ In $ Cons (f a) (outL (fmap f (L l)))

instance (Show a) => Show (List a) where
  show = show . outL

exampleList_1 :: List Int
exampleList_1 = L $ In $ Cons 10 $ In $ Cons 20 $ In Nil

newtype ListComp a = LC (Fix (G (ListF a)))
outLC :: ListComp a -> Fix (G (ListF a))
outLC (LC x) = x

instance (Show a) => Show (Fix (ListF a)) where
  show = show . out

instance (Show b, Show a) => Show (ListF a b) where
  show Nil = "Nil"
  show (Cons a l) = "Cons " ++ show a ++ " $ " ++ show l

instance Show a => Show (Fix (G (ListF a))) where
  show = show . out

instance Show a => Show (ListComp a) where
  show = show . outLC

exampleListComp_1 :: ListComp Int
exampleListComp_1 = LC $ In $ F $ Nil

exampleListComp_2 :: ListComp Int
exampleListComp_2 = LC $ In $ F $ Cons 10 $ In $ Step $ In $ F $ Nil

exampleListComp_3 :: ListComp Int
exampleListComp_3 = LC $ In $ Step $ In $ F $ Cons 10 $ In $ Step $ In $ F $ Nil

exampleListComp_inf_step :: ListComp Int
exampleListComp_inf_step = LC $ In $ Step $ outLC exampleListComp_inf_step

exampleListComp_inf_42 :: ListComp Int
exampleListComp_inf_42 = LC $ In $ F $ Cons 42 $ outLC exampleListComp_inf_42

-- has some numbers in it, then infinitely many computation steps
exampleListComp_some_then_infstep :: ListComp Int
exampleListComp_some_then_infstep = LC $ In $ F $ Cons 1 $ In $ F $ Cons 2 $ outLC exampleListComp_inf_step
