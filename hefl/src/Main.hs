{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Megaparsec
import Text.Megaparsec.Char

import EflTypes
import IrParser
import EflLog
import LogParser
import Deps (mkStmtInst, getDependencies, getDepEdges)
import Para
import IrPrinter
import System.Environment
import System.IO.Unsafe
import System.Exit

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc

import Control.DeepSeq
import Control.Monad.Trans.Reader
import Control.Monad

import Data.Maybe
import Data.Either
import Data.List

import Debug.Trace
import Data.Binary

import Safe

instance Binary Entry
instance Binary Type
instance Binary Var
instance Binary Expr
instance Binary Statement
instance Binary ARR_SIZE
instance Binary DimType
instance Binary DepLevel
instance Binary DependencyType
instance Binary Op
instance Binary Program
instance Binary DepEdge

instance NFData Entry
instance NFData Type
instance NFData Var
instance NFData Expr
instance NFData Statement
instance NFData ARR_SIZE
instance NFData DimType
instance NFData DepLevel
instance NFData DependencyType
instance NFData Op
instance NFData Program
instance NFData DepEdge
--instance NFData CMState


getLogEntries = do
  addLoopInfo . parseLogs <$> readFile "output\\log.f90.log"

getInsts =
  mkStmtInst <$> getLogEntries

getDeps = do
  logEntries <- getLogEntries
  let statements = S.toList . S.fromList . map logstmt $ logEntries
  let deps = getDependencies . mkStmtInst $ logEntries
  let edges = getDepEdges deps
  let depGraph = (statements, edges)
  return depGraph

data Mode = BuildLogEfl | Par | ShowDeps


pprDepGraph :: DepGraph -> Doc a
pprDepGraph (nodes', edges') =
  let mkNode n = "s" <> pretty n <> ";"
      nodes = vcat $ map mkNode nodes'
      edges = vcat $ map pretty edges'
  in
  "digraph G {" <> nest 2 (hardline <> vcat [nodes, edges]) <>
  "}"
  

mkDepGraph :: IO ()
mkDepGraph = do
  graph <- getDeps
  putStrLn . show .   pprDepGraph $ graph


showUsage :: a
showUsage =
  unsafePerformIO $ die $
    "\n\tInvalid arguments!" ++
    "\n\t\tUsage: \t<-log|-par|-deps> <file>\n\n"++
    "\teg:" ++
    "\t./hefl -log input/test.ir\n"

getMode :: [String] -> Mode
getMode args
  | "-log" `elem` args = BuildLogEfl
  | "-par" `elem` args = Par
  | "-deps" `elem` args = ShowDeps
  | otherwise = showUsage

mkPar mFile = do
  putStrLn "Vectorizing"
  ir <- getIr $ fromMaybe "test1.ir" mFile
  deps <- getDeps
  let header = fortranHeader <> pprPrgHeader ir
  let state = vectorizeStatements ir deps
  let loopCode = cmCode state
  let footer = pprPrgFooter ir
  let progDoc = header <> loopCode <> footer

  putStrLn . layoutLongLines $ progDoc

main = do
  args <- getArgs
  let mode = getMode args

  case mode of
    BuildLogEfl ->
      if length args < 2 then showUsage
        else void $ mkLoggingIr (args !! 1)
    Par ->
      if length args == 1
        then mkPar Nothing
        else mkPar (Just $ args !! 1)
    ShowDeps ->
      mkDepGraph
  --encodeFile "cache.bin" (ir,deps)
  --(ir, deps) <- decodeFile "cache.bin" :: IO (Program, DepGraph)

  --traceM "Vectorizing"
  --let state = vectorizeStatements ir deps
  --return state

getIr :: FilePath -> IO Program
getIr file = do
  s <- readFile file
  let readerM = runParserT program "Foo" s :: Reader SymbolMap (Either (ParseError Char String) Program)
  let result = runReader readerM M.empty :: (Either (ParseError Char String) Program)
  return $ fromRight (error "Failed to parse IR/program") result :: IO Program

mkLoggingIr file = do
  parsedAst <- getIr file
  putStrLn "Annotating log commands"
  --print result

  let withLogging = addLogStatements parsedAst
  let fortran_seq = pprEfl withLogging
  writeFile "output/log.f90" fortran_seq
  --print withLogging
  return withLogging
