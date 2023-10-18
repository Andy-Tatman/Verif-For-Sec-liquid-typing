module Logic
  ( 
    Logic (..)
  , neg
  , true
  , false
  , and
  , or
  , implies  
  ) where

import Expr
import Prelude hiding (and, or)

-- The constraints that we generate:
data Logic a 
    = CompL (Comparison a)
    | NegL (Logic a)
    | And [Logic a]
    deriving (Eq, Ord, Show)

instance Semigroup (Logic a) where
  lhs <> rhs = and [lhs, rhs]

-- | Negation (removes double negatives)
neg :: Logic a -> Logic a
neg (NegL l) = l
neg l = NegL l

-- | True
true :: Logic a
true = And []

-- | False
false :: Logic a
false = NegL true

-- | Conjunction (removes nested conjuncts)
and :: [Logic a] -> Logic a
and = unflatten . mconcat . (flatten <$>)
  where
    unflatten [s] = s
    unflatten s = And s

    flatten (And s) = s
    flatten s = [s]

-- | Disjunction
or :: [Logic a] -> Logic a
or = neg . and . (neg <$>)

-- | Implication
implies :: Logic a -> Logic a -> Logic a
implies lhs rhs = neg $ and [lhs, neg rhs]

instance Subable Logic where
    subst x y (CompL z) = CompL $ subst x y z 
    subst x y (NegL z) = NegL $ subst x y z 
    subst x y (And z) = And $ map (subst x y) z
  