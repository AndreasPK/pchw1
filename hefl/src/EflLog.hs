{-# LANGUAGE RecordWildCards #-}

module EflLog where

import EflTypes as ET
import EflUtils
import Control.Monad
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

import Text.Printf
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Foldable
import Data.List
import Data.Maybe

import qualified Data.Set as Set

import Debug.Trace

exprVars :: Expr -> [Var]
exprVars e = foldExpr (\l e -> l <> getVar e) [] e
    where
        getVar :: Expr -> [Var]
        getVar (VarExpr v) = [v]
        getVar _ = []

getDepInfo :: Statement -> (Int, Maybe Var, [Var])
getDepInfo (Assign nr v@(Var _ idxs) rhs) =
    (nr, Just v, concatMap (foldMapExpr exprVars) (rhs:idxs))
getDepInfo (If {stmtNr = nr, cond = cond}) =
    (nr, Nothing, exprVars cond)
getDepInfo (For {stmtNr = nr, ..}) =
    (nr, Nothing, concatMap (foldMapExpr exprVars) [lb, ub, step])
getDepInfo (Write exprs) =
    (0, Nothing, concatMap (foldMapExpr exprVars) exprs)

mkLogStatements :: Statement -> [Statement]
mkLogStatements s =
    case s of
        Assign {} ->
            writeExpr " DEF " (fromJust written) :
            nub (map (writeExpr " USE ") read ++ [s])
        If {} ->
            nub $ map (writeExpr " USE ") read ++ [s]
        For {loopvar = lvEntry, body = body} ->
            --WriteExpr (write(*,*) '901 i loop begin')
            Write [mkStrLit $ lblNr ++ varName lvEntry ++ " loop_begin"] :
            nub (map (writeExpr " USE ") read) ++
            [markIterations s, Write [mkStrLit $ lblNr ++ varName lvEntry ++ " loop_end"]]
        Write {} -> [s]
  where
    (nr, written, read) = getDepInfo s
    lblNr = printf "%03d " nr
    varInfo s v = lblNr ++
                    (maybe (error "No Entry info") varName . entry $ v) ++
                    s
    writeExpr :: String -> Var -> Statement
    writeExpr s v = Write $ mkStrLit (varInfo s v) : (indexExprs v)

    markIterations :: Statement -> Statement
    markIterations s@(For{loopvar = entry, body = body}) =
        s{body = (iteration_start:body) ++ [iteration_end]}
      where
        iteration_start =
            Write [mkStrLit (lblNr ++ varName entry ++ " iteration_start ")
                , mkVarExprFromEntry entry]
        iteration_end =
            Write [mkStrLit (lblNr ++ varName entry ++ " iteration_end ")
                , mkVarExprFromEntry entry]

addLogStatements :: Program -> Program
addLogStatements prog@(Program {stmts = eflStmts}) =
    let stmts = foldMap (updateStatements mkLogStatements) eflStmts
    in prog {stmts = stmts}