{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
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
--import Control.Monad.State.Class

import Data.Char

import Debug.Trace

type Indicies = [Int]
data Use
    = Def | Use -- Variable access
    | LoopStart | LoopEnd --Loop Statement
    | IterStart | IterEnd --Start/end of iteration
    deriving (Eq,Ord,Show)

data LogEntry a = LogEntry
    { logstmt :: Int
    , logId :: Id
    , logUse :: Use
    , logIndicies :: [Int] --Iterations for each level of loop
    , loopInfo :: a
    } deriving (Eq,Ord,Show,Functor)

matchLogEntries :: Ord a => [LogEntry a] -> M.Map (Id,Indicies) [LogEntry a]
matchLogEntries [] = M.empty
matchLogEntries (l:logs)
  | logUse l /= Def && logUse l /= Use
  = m
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
puse "iteration_start" = IterStart
puse "iteration_end" = IterEnd
puse other = error $ "Invalid use field:" ++ other


parseLogs :: String -> [LogEntry ()]
parseLogs = map readLogLine . lines

readLogLine :: String -> LogEntry ()
readLogLine s =
    let (label:var:use:idxs) = words s
    in
    LogEntry (read label) var (puse use) (map read idxs) ()

data LoopInfo = LoopInfo
    {   loopLevel :: Int
    ,   loopIds :: [Int] --inner first
    ,   loopStatement :: Int
    ,   loopIterations :: [Int] --current iteration vector, inner first
    }
    deriving (Eq,Show,Ord)

type LoopState = State [LoopInfo]

updateLoopInfo :: Show a => LogEntry a -> LoopState()
updateLoopInfo entry@(LogEntry _stmt _var use _inds _)
    | use == Use || use == Def = return ()
updateLoopInfo entry@(LogEntry stmt _var use inds _)
    | use == LoopStart
    = do
        oldInfo <- get
        let (indices,iterations)
                | (e:_) <- oldInfo
                = (stmt:loopIds e, loopIterations e)
                | otherwise   = ([stmt], [])
        let level = length indices
        let newInfo = LoopInfo level indices stmt iterations
        put (newInfo:oldInfo)
    | use == LoopEnd
    = modify' (drop 1)
    | use == IterStart
    = do
        (li:ls) <- get
        put $ li{loopIterations = head inds:loopIterations li}:ls
    | use == IterEnd
    = modify' (\(x:xs) -> x {loopIterations = drop 1 (loopIterations x)} : xs)


addLoopInfo :: Show a => [LogEntry a] -> [LogEntry (Maybe LoopInfo)]
addLoopInfo entries =
    flip evalState [] $ mapM setInfo entries
  where
    setInfo :: Show a => LogEntry a -> LoopState (LogEntry (Maybe LoopInfo))
    setInfo entry@(LogEntry stmt var use inds _) = do
        oldInfo <- get
        updateLoopInfo entry
        return $ fmap (listToMaybe . const oldInfo) entry



