{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Deps
where

import EflTypes as ET
import LogParser as LP

import Data.Bifunctor
import Data.Foldable
import Data.List
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

import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

data StmtInst = StmtInst
    { stmtId :: StatmentId
    , loopInfo :: Maybe LoopInfo --looplevel/id
    , varUsed :: [(Id,[Int])] --vars used and indices
    , varDef :: Maybe (Id,[Int]) --var written
    }
    deriving (Eq,Ord,Show)

sourceOrder :: StmtInst -> StmtInst -> Ordering
sourceOrder (StmtInst {stmtId = id1}) (StmtInst {stmtId = id2}) =
    compare id1 id2


--Assuming S1 gets executed before S2 does S2 depend on S1?
hasDataDep :: StmtInst -> StmtInst -> Maybe Dependency
hasDataDep (StmtInst id1 loop1 used1 def1)
           (StmtInst id2 loop2 used2 def2)
  | Just def1' <- def1
  , def1' `elem` used2
  = Just $ Dependency (id1, id2) TRUE
  | Just def2' <- def2
  , def2' `elem` used1
  = Just $ Dependency (id1, id2) ANTI
  | isJust def1
  , def1 == def2
  = Just $ Dependency (id1, id2) OUT
  | otherwise
  = Nothing

mkStmtInst :: [(LogEntry (Maybe LoopInfo))] -> [StmtInst]
mkStmtInst [] = []
mkStmtInst (l:ls)
  | use /= Use && use /= Def
  = mkStmtInsts ls
  | otherwise
  = StmtInst (logstmt l) li varUses (listToMaybe varDefs) :
    mkStmtInsts rest
    where
      use = LP.logUse l
      li = LP.loopInfo l :: Maybe LoopInfo
      sameInfo (LogEntry { logstmt = stmtId1, loopInfo = loopInfo1 })
               (LogEntry { logstmt = stmtId2, loopInfo = loopInfo2 })
            = stmtId1 == stmtId2 && loopInfo1 == loopInfo2
      (grp, rest) = span (sameInfo l) (l:ls)
      varDefs,varUses :: [(Id,[Int])]
      (varDefs,varUses) = bimap toUsePair toUsePair .
                          partition (\x -> logUse x == Def) $ grp
      toUsePair :: [LogEntry (Maybe LoopInfo)] -> [(Id,[Int])]
      toUsePair entries =
        map go entries
        where
          go LogEntry{logId = logId, logIndicies = indices}
            = (logId,indices)

--findDependency :: LogEntry ->

