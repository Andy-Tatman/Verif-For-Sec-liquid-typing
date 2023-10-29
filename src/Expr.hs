module Expr
  ( 
    Expr (..)
  , BinOp (..)
  , Comparison (..)
  , CompOp (..)
  , Pred (..)
  , Vars (..)
  , Subable (..)  
  , Function (..)
  , Statement (..)
  , Type (..)
  , RefineType (..)
  ) where

-- This file has been adapted from the Expr.hs file in the following repository:
-- https://github.com/Verification-for-Security/vc-gen

import Data.Set (Set)
import qualified Data.Set as Set

data Function a 
 = Func {
  -- -- Func name
  -- fname :: a,
  -- Bound variable which has type fpre.:
  fvar :: a,
  -- Type of param.:
  fpre :: Type a,
  -- Type of final expression:
  fpost :: Type a,
  -- Bound var (again) which has type fpre.: (fbound MUST equal fvar!!!)
  fbound :: a,
  -- List of statements
  fbody :: [Statement a],
  -- Return expression
  fret :: Expr a
 }
 deriving (Eq, Ord, Show)

data Type a  
  = Simple (RefineType a) -- Int{r}
  | FuncType (a) (Type a) (Type a) -- x:t -> t, where x may occur in t2.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data RefineType a 
  = Rt (a) (Pred a) -- {v|p}, where v may occur in p (if p != a bool, it probably should).
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Statement a 
  = Expr (Expr a)
  | LetAssign (a) (Type a) (Expr a) -- A special kind of 'expression'.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- type Var = String 

-- | Expressions are either of type integer or array
data Expr a
  = Variable a
  | ConstI Integer
  | BinOp BinOp (Expr a) (Expr a)
  | Minus (Expr a)
  | If (Pred a) (Expr a) (Expr a)
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
  = Conj (Pred a) (Pred a) -- &
  | Disj (Pred a) (Pred a) -- |
  | ConstB Bool
  | Neg (Pred a)           -- !
  | Comp (Comparison a)
  -- | IfElse (Pred a) (Pred a) (Pred a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary expression comparisons
data Comparison a
  = Compar CompOp (Expr a) (Expr a) 
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data CompOp 
  = LEQ -- <=
  | LE  -- <
  | GEQ -- >=
  | GE  -- >    
  | EQU -- == -- Named EQU to avoid Ambigious name with Prelude
  | NEQ -- !=
  deriving (Eq, Ord, Show)


class Vars f where
  vars :: Ord a => f a -> Set (Expr a)

instance Vars Expr where
  vars = \case
    v@(Variable _) -> Set.singleton v
    ConstI _ -> mempty
    BinOp _ lhs rhs -> vars lhs <> vars rhs
    Minus e -> vars e
    If p lhs rhs -> vars p <> vars lhs <> vars rhs

instance Vars Statement where 
  vars = \case
    Expr e -> vars e
    LetAssign v t e -> Set.singleton (Variable v) <> vars t <> vars e

instance Vars Type where 
  vars = \case 
    Simple r -> vars r
    FuncType _ _ _ -> undefined

instance Vars RefineType where 
  vars (Rt v p) = Set.singleton (Variable v) <> vars p

instance Vars Pred where
  vars = \case
    Conj lhs rhs -> vars lhs <> vars rhs
    Disj lhs rhs -> vars lhs <> vars rhs
    ConstB _ -> mempty
    Neg x -> vars x
    Comp c -> vars c
    -- IfElse left middle right -> vars left <> vars middle <> vars right

instance Vars Comparison where
  vars = \case 
    Compar _ lhs rhs -> vars lhs <> vars rhs 


-- | Substitues an expression for the passed variable
class Subable s where
  subst :: Eq a => a -> Expr a -> s a -> s a

instance Subable Expr where
  subst lookingFor newReplacement (Variable a) = if a == lookingFor then newReplacement else Variable a
  subst _ _ (ConstI a) = ConstI a
  subst x y (BinOp oper left right) = BinOp oper (subst x y left) (subst x y right)
  subst x y (Minus a) = Minus $ subst x y a
  subst x y (If p left right) = If (subst x y p) (subst x y left) (subst x y right)

instance Subable Pred where
  subst x y (Conj left right) = Conj (subst x y left) (subst x y right)
  subst x y (Disj left right) = Disj (subst x y left) (subst x y right)
  subst _ _ (ConstB b) = ConstB b
  subst x y (Neg z) = Neg $ subst x y z
  subst x y (Comp z) = Comp $ subst x y z

instance Subable Comparison where
  subst x y (Compar c left right) = Compar c (subst x y left) (subst x y right)