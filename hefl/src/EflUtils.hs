{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module EflUtils where

import EflTypes as ET

mkStrLit :: String -> Expr
mkStrLit = StringLit

mkVarExprFromEntry :: Entry -> Expr
mkVarExprFromEntry entry =
    VarExpr (Var (Just entry) [])