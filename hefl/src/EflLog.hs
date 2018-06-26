module EflLog where

import EflTypes as ET
import Control.Monad
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

import Text.Printf
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Foldable

exprVars :: Expr -> [Var]
exprVars e = foldExpr (\l e -> l <> getVar e) [] e
    where
        getVar :: Expr -> [Var]
        getVar (VarExpr v) = [v]

getDepInfo :: Statement -> (Int, Maybe Var, [Var])
getDepInfo (Assign nr v@(Var _ idxs) rhs) =
    (nr, Just v, foldl' (\l e -> l <> exprVars e) [] (rhs:idxs))
getDepInfo (If {stmtNr = nr, cond = cond}) =
    (nr, Nothing, exprVars cond)

