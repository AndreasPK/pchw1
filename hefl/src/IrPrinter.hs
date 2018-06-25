{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module IrPrinter where

import EflTypes as ET
import Control.Monad
import Data.Text.Prettyprint.Doc

instance Pretty Expr where
    pretty = pprExpr

pprOp :: Op -> Doc a
pprOp PLUS = "+"
pprOp MINUS = "-"
pprOp MULT = "*"
pprOp DIV = "/"
pprOp ET.EQ = "=="
pprOp LE = "<="
pprOp ET.LT = "<"
pprOp GE = ">="
pprOp ET.GT = ">"
pprOp NE = "<>"
pprOp AND = ".and."
pprOp OR = ".or."
pprOp NOT = ".not."

pprId :: Id -> Doc a
pprId = pretty

pprType :: Type -> Doc a
pprType FLOAT = "float"
pprType INT   = "int"

pprArrSize :: ARR_SIZE -> Doc a
pprArrSize (ARR_SIZE _ ranges) =
    list $ map pprRange ranges
  where
    pprRange (from,to) =
        pretty from <> ":" <> pretty to

pprExprList :: [Expr] -> Doc a
pprExprList [] = mempty
pprExprList xs = list $ map pretty xs

pprExpr :: Expr -> Doc a
pprExpr = undefined

pprVar v@(Var _ indices) =
    pretty (getVarName v) <> list indices





