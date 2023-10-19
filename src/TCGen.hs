module TCGen
  ( 
    checker
  , tcgenTest 
  ) where

import Expr
import Logic
import SMT 

-- Predicates allow any formula; Logic needs to be CNF. 
convertPredToLogic :: Pred String -> Logic String
convertPredToLogic (Conj x y) = Logic.and [convertPredToLogic x, convertPredToLogic y]
convertPredToLogic (Disj x y) = Logic.or [convertPredToLogic x, convertPredToLogic y]
convertPredToLogic (ConstB b) = if b then true else false
convertPredToLogic (Neg p) = neg $ convertPredToLogic p
convertPredToLogic (Comp c) = CompL c

-- Obtain our constraints from the types provided.
-- This involves substituting the variable (potentially) used in the type with 
-- relevant variable or expression.
-- Example: x:Int{v|v >= 0} gives the logic string {x >= 0}.
typeToLogic :: Type String -> Expr String -> Logic String
typeToLogic (Simple (Rt oldVar oldPred)) newExpr = do
    let newPred = subst oldVar newExpr oldPred
    convertPredToLogic newPred
typeToLogic (FuncType _ _ _) _ = undefined -- TODO?

-- Search through the expression. If we find any division (x/y) or modulo (x%y)
-- operations, we add the following constraint to our logic: y != 0.
exprCheck :: Expr String ->  Logic String
exprCheck (Variable _) = true
exprCheck (ConstI _) = true
exprCheck (BinOp b left right) = do
    case b of 
        Div -> do 
            let newLogic = CompL (Compar NEQ right (ConstI 0))
            Logic.and [exprCheck left, exprCheck right, newLogic]
        Mod -> do
            let newLogic = CompL (Compar NEQ right (ConstI 0))
            Logic.and [exprCheck left, exprCheck right, newLogic]
        _ ->
            Logic.and [exprCheck left, exprCheck right]
exprCheck (Minus e) = exprCheck e



tcgenStat :: [Statement String] -> Logic String -> Logic String
tcgenStat [] oldLogic = oldLogic
tcgenStat (x : xs) oldLogic = do
    let newLogic = tcgenStat xs oldLogic
    case x of 
      Expr e -> do
        -- check expr for /, %
        Logic.and [newLogic, exprCheck e]
        
      LetAssign assignedVar typeV exprV -> do
        let newTypeLogic = typeToLogic (typeV) (exprV)
        let updatedLogic = subst assignedVar exprV newLogic
        Logic.and [updatedLogic, newTypeLogic, exprCheck exprV]

-- The test function used in "stack test"
tcgenTest :: Function String -> IO Bool 
tcgenTest func = do
    print "test"
    -- Checks whether the function correctly has fvar = fbound
    if (fvar func) /= (fbound func) 
    then do -- Error 
        print "Error: The variable bound to the initial type (<var>:Int{..}) must equal the variable bound to the function (\\<var>.)."
        return False 
    else do
        -- Generate the type checking verification conditions
        let initType = typeToLogic (fpre func) (Variable $ fvar func) 
        print "initType ="
        print initType
        let retType = Logic.and [typeToLogic (fpost func) (fret func), exprCheck (fret func)] 
        print "retType ="
        print retType
        let bodyType = tcgenStat (fbody func) retType 
        print "bodyType ="
        print bodyType
        
        -- Generate the type conditions:
        let typeConds = tcgenMain func 
        print typeConds

        -- Check validity with the SMT solver:
        result <- SMT.valid typeConds
        return result

-- The main function for obtaining the type check constraints from our function.
tcgenMain :: Function String -> Logic String 
tcgenMain func = do
    -- Generate the type checking verification conditions
    let initType = typeToLogic (fpre func) (Variable $ fvar func) 
    let retType = Logic.and [typeToLogic (fpost func) (fret func), exprCheck (fret func)] 
    let bodyType = tcgenStat (fbody func) retType 
    implies initType bodyType

-- The main function for generating type checking conditions. Takes in a function,
-- checks that the function is valid (fvar == fbound), and then generates the 
-- type conditions. 
checker :: Function String -> IO Bool
checker func = do
    -- Checks whether the function correctly has fvar = fbound
    if (fvar func) /= (fbound func) 
    then do -- Error 
        print "Error: The variable bound to the initial type (<var>:Int{..}) must equal the variable bound to the function (\\<var>.)."
        return False 
    else do
        -- Generate the type conditions:
        let typeConds = tcgenMain func 
        -- print typeConds

        -- Check validity with the SMT solver:
        result <- SMT.valid typeConds
        -- print "Result = "
        -- print result
        -- putStrLn $ "'" <> "path" <> "' is valid: " <> show result
        return result