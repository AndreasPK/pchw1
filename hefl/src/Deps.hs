{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Monad.Combinators
import Control.Monad.Trans.State.Strict
--import Control.Monad.State.Class

import Data.Char

import qualified Data.Map as M
import qualified Data.Set as S


import Debug.Trace


type IndexedVar = (Id,[Int])
data StmtInst = StmtInst
    { stmtId :: StatementId
    , instLoopInfo :: Maybe LoopInfo --looplevel/id
    , varUsed :: [IndexedVar] --vars used and indices
    , varDef :: Maybe IndexedVar --var written
    }
    deriving (Eq,Ord,Show)


getDepEdges :: forall a. [Dependency a] -> [DepEdge]
getDepEdges deps =
    go deps
  where
    go :: [Dependency a] -> [DepEdge]
    go = map mkEdge . M.toList . foldl' insEdge M.empty
      where
        insEdge :: M.Map (StatementId, StatementId) (S.Set (DependencyType, Int))
                      -> Dependency a
                      -> M.Map (StatementId, StatementId) (S.Set (DependencyType, Int))
        insEdge m (Dependency (from,to) dtype dlvl _ _) =
          M.insertWith S.union (from,to) (S.singleton (dtype,getLoopLvl dlvl)) m
    mkEdge :: ((StatementId, StatementId), S.Set (DependencyType, Int))
                      -> DepEdge
    mkEdge ((from,to), es)
      = DepEdge from to (S.toList $ es)

    getLoopLvl (Independent) = 0
    getLoopLvl (DepLevel xs)
      | Just lvl <- elemIndex P.LT xs
      = (lvl+1)
      | Just lvl <- elemIndex P.GT xs
      = (lvl+1)
      | otherwise
      = error "Invariant error - dependencies should be independend or LT/GT"



getDependencies :: [StmtInst] -> [Dependency ()]
getDependencies insts =
  getDeps insts
    where
  getDeps :: [StmtInst] -> [Dependency ()]
  getDeps [] = []
  getDeps (s:ss) =
    hasDataDep s S.empty ss <> getDeps ss
    where

      --Assuming S1 gets executed before S2 does S2 depend on S1?
      hasDataDep :: StmtInst -> S.Set IndexedVar -> [StmtInst]
                 -> [Dependency ()]
      hasDataDep _ _ [] = []
      hasDataDep s1@(StmtInst id1 loop1 used1 def1) covered
                (s2@(StmtInst id2 loop2 used2 def2) : stmts)
        = (catMaybes [trueDep, outDep , antiDep]) <> hasDataDep s1 nextCover stmts
        where
          nextCover = foldl' (flip S.insert) covered (maybeToList def2)
          distVec = getDistVector s1 s2
          trueDep
            | Just def1' <- def1
            , not (isCovered def1')
            , def1' `elem` used2
            = Just $ Dependency (id1, id2) TRUE distVec
                    (fst def1') ()
            | otherwise = Nothing
          antiDep
            | Just def2' <- def2
            , not (isCovered def2')
            , def2' `elem` used1
            = Just $ Dependency (id1, id2) ANTI distVec
                  (fst def2') ()
            | otherwise = Nothing
          outDep
            | isJust def2
            , def1 == def2
            , not . isCovered . fromJust $ def2
            = Just $ Dependency (id1, id2) OUT distVec
                  (fst $ fromJust def1) ()
            | otherwise = Nothing
          isCovered v = S.member v covered


getDistVector :: StmtInst -> StmtInst -> DepLevel
getDistVector s1 s2
  | (Just li1) <- instLoopInfo s1
  , (Just li2) <- instLoopInfo s2
  , s1 /= s2
  = checkSourceOrder $ zipWith compare iters1 $ iters2
  | otherwise = Independent
  where
    Just li1 = instLoopInfo s1
    Just li2 = instLoopInfo s2

    --consider source order
    checkSourceOrder xs
      | all (==P.EQ) xs
      , s1 < s2
      = Independent
      | otherwise = DepLevel xs
    --Are these the same loops
    ind1 :: [Int]
    ind1 = reverse . loopIds $ li1
    ind2 = reverse . loopIds $ li2
    --What iteration in which loop
    iters1 = reverse . loopIterations $ li1
    iters2 = reverse . loopIterations $ li2
    vecLength = length . takeWhile id . zipWith (==) ind1 $ ind2

--x <- getDependencies <$> getInsts
--mapM_ print $ sort . nub . filter (\x -> depStmts x == (2,5) && depType x == ANTI) $ x




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

