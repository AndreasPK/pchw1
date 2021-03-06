{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}




module Para
where

import EflTypes as ET
import LogParser as LP
import Deps
import IrPrinter

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
import Control.Monad.Trans.Reader

import Safe

import Data.Functor.Identity

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.Text.Prettyprint.Doc

import GHC.Generics

import Debug.Trace

-- | For now assume that all nodes are part of an dependency.
--getSCC :: DepGraph -> a
getSCC (nodes, edges) =
    let mkGraphEdge (DepEdge f t _) = (f, f, [t])
        mkGraphEdges m (DepEdge f t _) =
            M.insertWith (S.union) f (S.singleton t) m
        gnodes = M.fromList $ map (\k -> (k,S.empty)) nodes
        graph = map (\(k,v) -> (k,k,S.toList v)) . M.toList $ foldl' mkGraphEdges gnodes edges
    in reverse $ G.stronglyConnComp graph


depGraphAt :: DepGraph -> LoopLevel -> DepGraph
depGraphAt (nodes, edges) level =
    (nodes, filterConnections . map
                (\(DepEdge f t es) -> DepEdge f t (filter filterEdges es)) $
                edges)
  where
    filterEdges (_,lvl) = lvl > level || lvl == 0
    filterConnections =
        filter f
      where
        f (DepEdge f t es)
          | null es = False
          | all (`elem` nodes) [f,t]
          = True
          | otherwise
          = False

data CMState =
    CMState
    { cmPg :: Program
    , cmCode :: Doc ()
    , cmDeps :: DepGraph
    } deriving (Show, Generic)

type CM = State CMState
type LoopGenInfo = (Expr, Expr, Expr, Entry, StatementId)

lilb (lb,_,_,_) = lb
liub (_,ub,_,_) = ub
--listep (_,_,x,_) = x
livar (_,_,_,var) = var

instance (Pretty a1,Pretty a2,Pretty a3,Pretty a4) => Pretty (a1,a2,a3,a4) where
    pretty (e1,e2,e3,e4) =
        "(" <> pretty e1 <> "," <> pretty e2 <> "," <>
            pretty e3 <> "," <> pretty e4 <> ")"

instance (Pretty a1,Pretty a2,Pretty a3,Pretty a4,Pretty a5) => Pretty (a1,a2,a3,a4,a5) where
    pretty (e1,e2,e3,e4,e5) =
        "(" <> pretty e1 <> "," <> pretty e2 <> "," <>
            pretty e3 <> "," <> pretty e4 <> "," <> pretty e5 <> ")"

-- Get loops for statement, outer first
getLoops :: Program -> StatementId -> Maybe [Statement]
getLoops Program {stmts = stmts} id =
    listToMaybe $ mapMaybe go stmts
    where
    go stmt
      | Write {} <- stmt = Nothing
      | stmtNr stmt == id
      = Just []
      | If {} <- stmt
      = error "If parallelization not supported"
      | s@For  {body = body} <- stmt
      = fmap (s:) . listToMaybe $ mapMaybe go body
      | Assign {} <- stmt
      = Nothing

getStmt :: Program -> StatementId -> Maybe Statement
getStmt Program {stmts = stmts} id =
    listToMaybe $ mapMaybe go stmts
    where
    go stmt
      | Write {} <- stmt
      = Nothing
      | stmtNr stmt == id
      = Just stmt
      | If {} <- stmt
      = error "If parallelization not supported"
      | s@For  {body = body} <- stmt
      = listToMaybe $ mapMaybe go body
      | Assign {} <- stmt
      = Nothing

--Get lower/upper/step and loop variable
getLoopInfos :: StatementId -> CM (Maybe [LoopGenInfo])
getLoopInfos stmtId = do
    p <- getProgram
    let loops = getLoops p stmtId
    return $ map (\for -> (lb for, ub for, step for, loopvar for, stmtNr for)) <$> loops


runCM :: Program -> DepGraph -> CM a -> CMState
runCM prg graph action =
    execState action (CMState prg mempty graph)

getProgram :: CM Program
getProgram = cmPg <$> get

addCode :: Doc () -> CM ()
addCode doc = do
    state <- get
    put (state { cmCode = cmCode state <> doc})


vectorizeStatements :: Program -> DepGraph -> CMState
vectorizeStatements prg deps =
    let nodes = foldMap (foldStatements (\as x -> if isAssignment x then x:as else as) [])
                        (stmts prg)
        grph = (map stmtNr nodes, snd deps)
        nodeIds = map stmtNr nodes
    in
    runCM prg grph (vectorCode nodeIds 0)

vectorCode :: [StatementId] -> LoopLevel -> CM ()
vectorCode [] _ = return ()
vectorCode statements level = do
    -- traceM " "
    -- traceM $ "VectorCode: " ++ show level ++ show statements
    depGraph <- cmDeps <$> get
    let currentGraph = depGraphAt (first (const statements) depGraph) level
    --traceM . show $ foldMap (\x -> pretty x <> hardline) $ snd currentGraph
    let scc = getSCC currentGraph
    --traceM $ "scc: " ++ show scc

    mapM_ (generateSCC level) scc

checkLoopInfo :: Monad m => [[LoopGenInfo]] -> m Bool
checkLoopInfo (li:lis) =
    let x = map (\i2 -> head li == head i2) $ lis
    in if (and x)
        then return True
        else return False --(error $ "Missmatched loops not handled:" ++ show (pretty $ li:lis))

generateSCC :: LoopLevel -> G.SCC StatementId -> CM ()
generateSCC level (G.AcyclicSCC stmt) = generateStatement level stmt
generateSCC level (G.CyclicSCC stmts) = do
    allLoopInfos <- map (drop level) . catMaybes <$> mapM getLoopInfos stmts
{-    traceM . show $ "Generate SCC - Statements:" <> pretty stmts <> hardline <>
                    "level:" <> pretty level <> hardline <>
                    "loopInfo:" <> pretty allLoopInfos
-}
    matched <- checkLoopInfo allLoopInfos
    case matched of
        True -> do
            --We already dealth with <level> loops so get the outermost loop of this level
            let mloop = headMay $
                    drop level .
                    headNote "Expected a loop for circular dependency" $
                    allLoopInfos
            case mloop of
                Just loop ->generateMatchedLoops level stmts loop
                Nothing -> mapM_ (generateStatement level) stmts

        False -> do
            generateMissmatchedLoops level stmts allLoopInfos


generateMatchedLoops :: LoopLevel -> [StatementId] -> LoopGenInfo -> CM ()
generateMatchedLoops level stmts loopInfo = do
    -- genLoopHead
    let loopHead = runReader (pprForHead loopInfo) (level*4)
    addCode loopHead
    --Generate statements
    vectorCode stmts (level+1)
    -- genLoopTail
    let loopTail = runReader (withoutLabel $ "end do" <> hardline) (level*4)
    addCode loopTail

generateMissmatchedLoops :: LoopLevel -> [StatementId] -> [[LoopGenInfo]] -> CM ()
generateMissmatchedLoops level stmts loopInfos = do
    let loopId (_,_,_,_,loopId) = loopId
    let loopHeaders = map (listToMaybe) loopInfos
    let pairs = zip stmts loopHeaders
    let together = groupBy (\x y -> snd x == snd y) pairs :: [[(StatementId, Maybe LoopGenInfo)]]
    let groups = map (\xs@(x:_) -> (map fst xs, snd x)) together :: [([StatementId], Maybe LoopGenInfo)]
    mapM_ (generateMissmatchedGroup level) groups

generateMissmatchedGroup :: LoopLevel -> ([StatementId],Maybe LoopGenInfo) -> CM ()
generateMissmatchedGroup level (stmts, mLoopInfo)
    | Just loopInfo <- mLoopInfo
    = generateMatchedLoops level stmts loopInfo
    | otherwise
    = mapM_ (generateStatement level) stmts



pprForHead :: LoopGenInfo -> PrintState (Doc a)
pprForHead (lb,ub,step,var,_) = 
    withoutLabel $ "do" <+> pretty (varName var) <+> "=" <+> pretty lb <+>
             "," <+> pretty ub <+> "," <> pretty step <> hardline

generateStatement :: LoopLevel -> StatementId -> CM ()
generateStatement level stmtId = do
    p <- getProgram
    loopInfo <- drop level <$> (fromMaybe [] <$> (getLoopInfos stmtId))
    let stmt = fromMaybe (error "GenStmt: invalid") $! getStmt p stmtId
    let vectorized = vectorize stmt loopInfo
    --generate vector expression
{-    when (stmtId == 4) $
        traceM $ show ("Vectorized" <> hardline <>
            "level:" <> pretty level <+> "stmt:" <> pretty stmtId <> hardline <>
            pretty loopInfo <> hardline <>
            pretty stmt <> hardline <>
            pretty vectorized
            )-}
    addCode $ pprStatement' vectorized (level*4)

vectorize :: Statement -> [LoopGenInfo] -> Statement
vectorize (Assign nr lhs rhs) lgi =
    Assign nr lhs' rhs'
    where
      lhs' = lhs {indexExprs = map (`replaceAll` lgi) (indexExprs lhs)}
      rhs' = mapExpr (`replaceAll` lgi) rhs
      replaceAll :: Expr -> [LoopGenInfo] -> Expr
      replaceAll expr lgis = foldl' replaceVar expr lgis

      replaceVar (VarExpr v) (lb,ub,_step,varEntry,_)
        | getVarName v == varName varEntry
        = VecExpr lb ub
        | otherwise
        = VarExpr v
      replaceVar expr lgi = expr
