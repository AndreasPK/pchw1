{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Deps
where

import EflTypes as ET
import LogParser as LP

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
    { stmtId :: StatementId
    , loopInfo :: Maybe LoopInfo --looplevel/id
    , varUsed :: [(Id,[Int])] --vars used and indices
    , varDef :: Maybe (Id,[Int]) --var written
    }
    deriving (Eq,Ord,Show)

getDependencies :: [StmtInst] -> [Dependency]
getDependencies insts =
  concatMap getDeps checkList
    where
  checkList = tails insts
  getDeps :: [StmtInst] -> [Dependency]
  getDeps (s:ss) =
    filterCover (S.empty) $
    concatMap (hasDataDep s) ss
  getDeps [] = []

  --(StmtInst {stmtId = id1, varDef = def, varUsed = use})
  filterCover :: S.Set Id -> [Dependency] -> [Dependency]
  filterCover _       [] = []
  filterCover covered (dep@Dependency { depVar = depVar, depType = depType } : deps)
    --Already covered
    | S.member depVar covered
    = filterCover covered deps
    --If there is any anti or out dependency then that one covers any further dependencies
    | depType == ANTI || depType == OUT
    = dep : filterCover (S.insert depVar covered) deps
    | otherwise
    = dep : filterCover covered deps


getDistVector :: StatementId -> StatementId -> Maybe LoopInfo -> Maybe LoopInfo -> DepLevel
getDistVector _ _ Nothing _ = Independent
getDistVector _ _ _ Nothing = Independent
getDistVector s1 s2 (Just li1) (Just li2) =
  checkSourceOrder . take vecLength $ zipWith compare iters1 $ iters2
  where
    --consider source order
    checkSourceOrder xs
      | all (==P.EQ) xs
      , s1 < s2
      = Independent
      | otherwise = DepLevel xs
    --Are these the same loops
    ind1 = reverse . loopIndicies $ li1
    ind2 = reverse . loopIndicies $ li2
    --What iteration in which loop
    iters1 = reverse . loopIterations $ li1
    iters2 = reverse . loopIterations $ li2
    vecLength = length . takeWhile id . zipWith (==) ind1 $ ind2


--Assuming S1 gets executed before S2 does S2 depend on S1?
hasDataDep :: StmtInst -> StmtInst -> [Dependency]
hasDataDep (StmtInst id1 loop1 used1 def1)
           (StmtInst id2 loop2 used2 def2)
  = catMaybes $ [trueDep, outDep, antiDep] -- <> inputDeps
  where
    distVec = getDistVector id1 id2 loop1 loop2
    trueDep
      | Just def1' <- def1
      , def1' `elem` used2
      = Just $ Dependency (id1, id2) TRUE distVec (fst def1')
      | otherwise = Nothing
    antiDep
      | Just def2' <- def2
      , def2' `elem` used1
      = Just $ Dependency (id1, id2) ANTI distVec (fst def2')
      | otherwise = Nothing
    outDep
      | isJust def2
      , def1 == def2
      = Just $ Dependency (id1, id2) OUT distVec (fst $ fromJust def1)
      | otherwise = Nothing
    inputDeps =
      let vars1 = S.fromList . map fst $ used1
          dep id
            | S.member id vars1
            = Just $ Dependency (id1, id2) INPUT distVec id
            | otherwise = Nothing
      in map (dep . fst) used2

mkStmtInst :: [LogEntry (Maybe LoopInfo)] -> [StmtInst]
mkStmtInst [] = []
mkStmtInst (l:ls)
  | use /= Use && use /= Def
  = mkStmtInst ls
  | otherwise
  = StmtInst (logstmt l) li varUses (listToMaybe varDefs) :
    mkStmtInst rest
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

