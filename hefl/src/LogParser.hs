{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{--
    Parser for EFL IR format, white space senstive!
-}
module LogParser where

import EflTypes as ET

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

import Debug.Trace
import System.IO.Unsafe
import Control.Monad.IO.Class

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

import Control.Monad.Combinators
import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class

import Data.Char

import Debug.Trace

instance ShowErrorComponent () where
    showErrorComponent = const ""

type LogParser = Parsec () String -- ParsecT String String (State LoopLevel)

type Indicies = [Int]
data Use = Def | Use | LoopStart | LoopEnd deriving (Eq,Ord,Show)
data LogEntry = LogEntry Int Id Use Indicies deriving (Eq,Ord,Show)



puse :: String -> Use
puse "DEF" = Def
puse "USE" = Use
puse "loop_begin" = LoopStart
puse "loop_end" = LoopEnd


readUse :: String -> LogEntry
readUse s =
    let (label:var:use:idxs) = words s
    in
    LogEntry (read label) var (puse use) (map read idxs)

data Dependency = Dependency
    { depStmts :: (Int,Int)
    , depType :: DependencyType
    , depLevel = Int
    } deriving (Eq, Ord, Show)

--Identified by position and loop variable
type LoopLevels = [Id]





