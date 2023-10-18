module TCGen
  ( 
    tcgenMain
  , tcgenTest 
  , checker
  ) where

import Logic
import Expr

-- tcgenTest :: Function String -> Logic String 
-- tcgenTest = undefined

-- tcgenMain :: Function String -> Logic String 
-- tcgenMain = undefined

convertPredToLogic :: Pred String -> Logic String
convertPredToLogic (Conj x y) = Logic.and [convertPredToLogic x, convertPredToLogic y]
convertPredToLogic (Disj x y) = Logic.or [convertPredToLogic x, convertPredToLogic y]
convertPredToLogic (ConstB b) = if b then true else false
convertPredToLogic (Neg p) = neg $ convertPredToLogic p
convertPredToLogic (Comp c) = CompL c

typeToLogic :: Type String -> Expr String -> Logic String
typeToLogic (Simple (Rt oldVar oldPred)) newExpr = do
    let newPred = subst oldVar newExpr oldPred
    convertPredToLogic newPred
-- TODO?
typeToLogic (FuncType _ _ _) _ = undefined

exprCheck :: Expr String ->  Logic String
exprCheck (Variable _) = true
exprCheck (ConstI _) = true
exprCheck (BinOp b left right) = do
    case b of 
        Div -> do 
            let newLogic = CompL (NEQ right (ConstI 0))
            Logic.and [exprCheck left, exprCheck right, newLogic]
        Mod -> do
            let newLogic = CompL (NEQ right (ConstI 0))
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


tcgenTest :: Function String -> IO (Bool) 
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
        let typeConds = implies initType bodyType
        print typeConds
        return True

tcgenMain :: Function String -> Logic String 
tcgenMain func = do
    -- Generate the type checking verification conditions
    let initType = typeToLogic (fpre func) (Variable $ fvar func) 
    let retType = Logic.and [typeToLogic (fpost func) (fret func), exprCheck (fret func)] 
    let bodyType = tcgenStat (fbody func) retType 
    implies initType bodyType

checker :: Function String -> IO (Bool)
checker func = do
    print "test"
    -- Checks whether the function correctly has fvar = fbound
    if (fvar func) /= (fbound func) 
    then do -- Error 
        print "Error: The variable bound to the initial type (<var>:Int{..}) must equal the variable bound to the function (\\<var>.)."
        return False 
    else do
        let typeConds = tcgenMain func 
        -- TODO: SMT Solver-ing...
        return True