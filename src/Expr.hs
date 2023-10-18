module Expr
  ( 
    Expr (..)
  , BinOp (..)
  , Comparison (..)
  , Pred (..)
  -- , Vars (..)
  , Subable (..)  
  , Function (..)
  -- , Var (..)
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

-- newtype Variable a = Var a deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

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
  = Conj (Pred a) (Pred a) -- &
  | Disj (Pred a) (Pred a) -- |
  | ConstB Bool
  | Neg (Pred a)           -- !
  | Comp (Comparison a)
  -- | IfElse (Pred a) (Pred a) (Pred a)
  -- | Func (Pred a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binary expression comparisons
data Comparison a
  = LEQ (Expr a) (Expr a) -- <=
  | LE  (Expr a) (Expr a) -- <
  | GEQ (Expr a) (Expr a) -- >=
  | GE  (Expr a) (Expr a) -- >    
  | EQU (Expr a) (Expr a) -- == -- Named EQU to avoid Ambigious name with Prelude
  | NEQ (Expr a) (Expr a) -- !=
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- data Constraint a
--   = Pred (Pred a)
--   | ConjC (Constraint a) (Constraint a)
--   | Impl (Pred a) (Pred a) (Constraint a)  -- For all x of type b: p implies c
--   deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- class Vars f where
--   vars :: Ord a => f a -> Set (Expr a)

-- instance Vars Expr where
--   vars = \case
--     v@(Variable _) -> Set.singleton v
--     ConstI _ -> mempty
--     BinOp _ lhs rhs -> vars lhs <> vars rhs

-- instance Vars Pred where
--   vars = \case
--     Conj lhs rhs -> vars lhs <> vars rhs
--     Disj lhs rhs -> vars lhs <> vars rhs
--     ConstB _ -> mempty
--     Neg x -> vars x
--     CompOp _ lhs rhs -> vars lhs <> vars rhs
--     -- IfElse left middle right -> vars left <> vars middle <> vars right
--     -- Func x -> vars x 
    

-- instance Vars Pred where
--   vars = \case
--     lhs :==: rhs -> vars lhs <> vars rhs
--     lhs :>=: rhs -> vars lhs <> vars rhs
--     lhs :<=: rhs -> vars lhs <> vars rhs

-- | Substitues an expression for the passed variable
class Subable s where
  subst :: Eq a => a -> Expr a -> s a -> s a

-- instance Subable Type where
  -- subst x y (Simple z) = Simple $ subst x y z
  -- subst _ _ (FuncType _ _ _) = undefined -- TODO?
    -- FuncType (x) (subst lookingFor newReplacement y) (if lookingFor == x then z else subst lookingFor newReplacement z)

-- instance Subable RefineType where
  -- subst lookingFor newReplacement (Rt variableV predi) = Rt (variableV) (if lookingFor == variableV then predi else subst lookingFor newReplacement predi)

-- instance Subable Statement where
  -- subst = undefined -- We do not do substitutions on statements.

  -- subst lookingFor newReplacement (Expr a) = Expr $ subst lookingFor newReplacement a
  -- subst lookingFor newReplacement (LetAssign assignV typeV exprAss) = undefined


-- instance Subable Var where
--   subst = undefined

instance Subable Expr where
  subst lookingFor newReplacement (Variable a) = if a == lookingFor then newReplacement else Variable a
  subst _ _ (ConstI a) = ConstI a
  subst x y (BinOp oper left right) = BinOp oper (subst x y left) (subst x y right)
  subst x y (Minus a) = Minus $ subst x y a

instance Subable Pred where
  subst x y (Conj left right) = Conj (subst x y left) (subst x y right)
  subst x y (Disj left right) = Disj (subst x y left) (subst x y right)
  subst _ _ (ConstB b) = ConstB b
  subst x y (Neg z) = Neg $ subst x y z
  subst x y (Comp z) = Comp $ subst x y z

instance Subable Comparison where
  subst x y (LEQ left right) = LEQ (subst x y left) (subst x y right)
  subst x y (LE left right) = LE (subst x y left) (subst x y right)
  subst x y (GEQ left right) = GEQ (subst x y left) (subst x y right)
  subst x y (GE left right) = GE (subst x y left) (subst x y right)
  subst x y (EQU left right) = EQU (subst x y left) (subst x y right)
  subst x y (NEQ left right) = NEQ (subst x y left) (subst x y right)
  
-- instance Subable Constraint where
--   subst x y (Pred p)= Pred $ subst x y p
--   subst x y (ConjC left right)= ConjC (subst x y left) (subst x y right)
--   -- TODO: What if boundThing is bound???
--   subst x y (Impl boundThing middle right)= Impl (subst x y boundThing) (subst x y middle) (subst x y right)
  
