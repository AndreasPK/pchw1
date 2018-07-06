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
data LogEntry = LogEntry
    { logstmt :: Int
    , logId :: Id
    , logUse :: Use
    , logIndicies :: [Int]
    }

findDeps :: [LogEntry] -> [LogEntry]
findDeps logs =
  where
    go :: [LogEntry] -> M.Map (Id,Indicies) [LogEntry]
    go [] m = m
    go (l:logs) m =
      where
        key = (logId l, logIndicies l)

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
    , depLevel :: Int
    } deriving (Eq, Ord, Show)

--Identified by position and loop variable
type LoopLevels = [Id]





