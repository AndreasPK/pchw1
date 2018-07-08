{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Para
where

import EflTypes as ET
import LogParser as LP
import Deps

import Prelude as P
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Semigroup
import Control.Monad

import Debug.Trace
import System.IO.Unsafe
import Control.Monad.IO.Class

import Control.Monad.Combinators
import Control.Monad.Trans.State.Strict
--import Control.Monad.State.Class

import Data.Char

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Text.Prettyprint.Doc

import Debug.Trace

-- | For now assume that all nodes are part of an dependency.
--getSCC :: DepGraph -> a
getSCC (_, edges) =
    let mkGraphEdge (DepEdge f t _) = (f, f, [t])
        mkGraphEdges m (DepEdge f t _) =
            M.insertWith (S.union) f (S.singleton t) m
        graph = map (\(k,v) -> (k,k,S.toList v)) . M.toList $ foldl' mkGraphEdges M.empty edges
    in reverse $ G.stronglyConnComp graph






