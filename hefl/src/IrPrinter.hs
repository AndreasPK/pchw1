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
import Data.Text.Prettyprint.Doc.Render.String

import Text.Printf
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

type PrintState = Reader Int

instance Pretty Expr where
    pretty = pprExpr

shift :: Int -> Doc a
shift i = pretty (replicate i ' ' :: String)

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

pprVar :: Var -> Doc a
pprVar v@(Var _ indices) =
    pretty (getVarName v) <> pprExprList indices

pprExprList :: [Expr] -> Doc a
pprExprList [] = mempty
pprExprList xs = list $ map pretty xs

pprExpr :: Expr -> Doc a
pprExpr (FloatLit l)    = pretty l
pprExpr (IntLit i)      = pretty i
pprExpr (VarExpr v)     = pprVar v
pprExpr (ParensExpr e)  = parens $ pretty e
pprExpr (OpExpr op e1 me1)
    | Just e2 <- me1
    = pretty e1 <> pprOp op <> pretty e2
    | Nothing <- me1
    = pprOp op <> pretty e1

pprLabel :: Int -> PrintState (Doc a)
pprLabel n = do
    indent <- ask
    return $ pretty (printf "%03d" n :: String) <> shift (indent +1)

--For lines with no label
withoutLabel :: Doc a -> PrintState (Doc a)
withoutLabel doc = do
    indent <- ask
    return $ shift (indent +4) <> doc

--Print statements with increasing indentation
pprStatementM :: Statement -> PrintState (Doc a)
pprStatementM (Assign n v rhs) = do
    lbl <- pprLabel n
    return $ lbl <> pprVar v <+> "=" <+> pretty rhs <> hardline
pprStatementM (If n cond t f) = do
    lbl <- pprLabel n
    stmtsTrue <- vsep <$> local (+4) (mapM pprStatementM t)
    stmtsFalse <- vsep <$> local (+4) (mapM pprStatementM t)
    elsed <- withoutLabel "else"
    endd <- withoutLabel "end if"
    let fdoc
            | null f = mempty
            | otherwise = elsed <> hardline <> stmtsFalse

    return $ lbl <> "if" <+> parens (pretty cond) <+> "then" <> hardline <>
        stmtsTrue <> fdoc <> endd
pprStatementM (For n var lb ub step stmts) = do
    lbl <- pprLabel n
    body <- vsep <$> local (+4) (mapM pprStatementM stmts) :: PrintState (Doc a)
    end <- withoutLabel "end do"

    return $ lbl <+> "do" <+> (pretty $ varName var) <+> "=" <+> pretty lb <+>
             "," <+> pretty ub <+> "," <> pretty step <> hardline <> body <>
                end <> hardline

--Print with indent zero
pprStatement :: Statement -> Doc a
pprStatement s = runReader (pprStatementM s) 0

pprProgram :: Program -> Doc a
pprProgram (Program name _syms stmts) =
    "! Compilers for Parallel Systems" <> hardline <>
    "! 185.A64 SS 2018 A. Klebinger" <> hardline <>
    "! Generated from EFL IR" <> hardline <>
    hardline <>
    "program" <+> pretty name <> hardline <>
    hardline <>
    "todo: definitions" <> hardline <>
    hardline <>
    (vsep $ map pprStatement stmts) <>
    hardline <>
    "end program" <+> pretty name <> hardline

pprEfl =
    renderString . layoutSmart (LayoutOptions Unbounded) . pprProgram



