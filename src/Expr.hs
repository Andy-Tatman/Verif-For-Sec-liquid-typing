module Expr
  ( 
    -- Expr (..)
  -- , 
  BinOp (..)
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
  | ConstB Bool
  | ConstI Integer
  | BinOp BinOp (Expr a) (Expr a)
--   | Array a
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
  -- = Expr a :==: Expr a
  -- -- ^ Equals
  -- | Expr a :>=: Expr a
  -- -- ^ Less than or equals
  -- | Expr a :<=: Expr a
  -- -- ^ Greater than or equals
  = Conj (Pred a) (Pred a)
  | Disj (Pred a) (Pred a)
  | Neg (Pred a)
  | IfElse (Pred a) (Pred a) (Pred a)
  | Func (Pred a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Constraint a
  = Pred (Pred a)
  | ConjC (Constraint a) (Constraint a)
  | Impl (Pred a) (Pred a) (Constraint a)  -- For all x of type b: p implies c
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

class Vars f where
  vars :: Ord a => f a -> Set (Expr a)

instance Vars Expr where
  vars = \case
    v@(Var _) -> Set.singleton v
    ConstB _ -> mempty
    ConstI _ -> mempty
    BinOp _ lhs rhs -> vars lhs <> vars rhs

instance Vars Pred where
  vars = \case
    Conj lhs rhs -> vars lhs <> vars rhs
    Disj lhs rhs -> vars lhs <> vars rhs
    Neg x -> vars x
    IfElse left middle right -> vars left <> vars middle <> vars right
    Func x -> vars x 
    

-- instance Vars Pred where
--   vars = \case
--     lhs :==: rhs -> vars lhs <> vars rhs
--     lhs :>=: rhs -> vars lhs <> vars rhs
--     lhs :<=: rhs -> vars lhs <> vars rhs

-- | Substitues an expression for the passed variable
class Subable s where
  subst :: Eq a => a -> Expr a -> s a -> s a

instance Subable Expr where
  subst lookingFor newReplacement (Var a) = if a == lookingFor then newReplacement else Var a
  subst _ _ (ConstB a) = ConstB a
  subst _ _ (ConstI a) = ConstI a
  subst x y (BinOp oper left right) = BinOp oper (subst x y left) (subst x y right)
  -- subst _ _ _ = undefined 

instance Subable Pred where -- TODO
  subst _ _ _ = undefined 
  
instance Subable Constraint where
  subst x y (Pred p)= Pred $ subst x y p
  subst x y (ConjC left right)= ConjC (subst x y left) (subst x y right)
  -- TODO: What if boundThing is bound???
  subst x y (Impl boundThing middle right)= Impl (subst x y boundThing) (subst x y middle) (subst x y right)
  
