{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{--
    Parser for EFL IR format, white space senstive!
-}
-- (parseLogs, findDeps, LogEntry(..)
-- , Dependency(..)    ) 
module LogParser 
where

import EflTypes as ET

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
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
    } deriving (Eq,Ord,Show)

matchLogEntries :: [LogEntry] -> M.Map (Id,Indicies) [LogEntry]
matchLogEntries [] = M.empty
matchLogEntries (l:logs) 
  | logUse l /= Def && logUse l /= Use
  = fmap (l:) m
  | otherwise
  = M.insertWith comb key [l] m
  where
    m = matchLogEntries logs
    comb [l1] old@(l2:_)
      | l1 == l2
      = old
      | otherwise = l1:old
    key :: (Id,Indicies)
    key = (logId l, logIndicies l)

puse :: String -> Use
puse "DEF" = Def
puse "USE" = Use
puse "loop_begin" = LoopStart
puse "loop_end" = LoopEnd

parseLogs :: String -> [LogEntry]
parseLogs = map readLogLine . lines

readLogLine :: String -> LogEntry
readLogLine s =
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





