module Expr
  ( Expr (..)
  , BinOp (..)
  , Pred (..)

  , Vars (..)
  , Subable (..)
  ) where

-- This file has been adapted from the Expr.hs file in the following repository:
-- https://github.com/Verification-for-Security/vc-gen

import Data.Set (Set)
import qualified Data.Set as Set

-- | Expressions are either of type integer or array
data Expr a
  = Var a
--   | Array a
  | Const Integer
  | BinOp BinOp (Expr a) (Expr a)
--   | Select (Expr a) (Expr a)
--   | Store (Expr a) (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary expression operations.
data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  deriving (Eq, Ord, Show)

-- | Predicates are of type boolean
data Pred a
  = Expr a :==: Expr a
  -- ^ Equals
  | Expr a :>=: Expr a
  -- ^ Less than or equals
  | Expr a :<=: Expr a
  -- ^ Greater than or equals
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class Vars f where
  vars :: Ord a => f a -> Set (Expr a)

instance Vars Expr where
  vars = \case
    v@(Var _) -> Set.singleton v
    -- a@(Array _) -> Set.singleton a
    Const _ -> mempty
    BinOp _ lhs rhs -> vars lhs <> vars rhs
    -- Select array index -> vars array <> vars index
    -- Store array index expr -> mconcat [vars array, vars index, vars expr]

instance Vars Pred where
  vars = \case
    lhs :==: rhs -> vars lhs <> vars rhs
    lhs :>=: rhs -> vars lhs <> vars rhs
    lhs :<=: rhs -> vars lhs <> vars rhs

-- | Substitues an expression for the passed variable
class Subable s where
  subst :: Eq a => a -> Expr a -> s a -> s a

instance Subable Expr where
  subst lookingFor newReplacement (Var a) = if a == lookingFor then newReplacement else Var a
--   subst lookingFor newReplacement (Array a) = if a == lookingFor then newReplacement else Array a
  subst _ _ (Const a) = Const a
  subst x y (BinOp oper left right) = BinOp oper (subst x y left) (subst x y right)
--   subst x y (Select left right) = Select (subst x y left) (subst x y right)
--   subst x y (Store left middle right) = Store (subst x y left) (subst x y middle) (subst x y right)
  -- subst _ _ _ = undefined
  
instance Subable Pred where
  subst x y (left :==: right)= subst x y left :==: subst x y right
  subst x y (left :>=: right)= subst x y left :>=: subst x y right
  subst x y (left :<=: right)= subst x y left :<=: subst x y right
  
