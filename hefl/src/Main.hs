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

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Reader
import Control.Monad

import Data.Either
import Data.List

getLogEntries = do
  addLoopInfo . parseLogs <$> readFile "output\\test.f90.log"

getInsts =
  mkStmtInst <$> getLogEntries

getDeps = do
  logEntries <- getLogEntries
  let statements = S.toList . S.fromList . map logstmt $ logEntries
  let deps = getDependencies . mkStmtInst $ logEntries
  let edges = getDepEdges deps
  let depGraph = (statements, edges)
  return depGraph

--main :: IO ()
main = do
  s <- readFile "input/test1.ir"
  let readerM = runParserT program "Foo" s :: Reader SymbolMap (Either (ParseError Char String) Program)
  let result = runReader readerM M.empty :: (Either (ParseError Char String) Program)
  let parsedAst = fromRight (error "Failed to parse program") result :: Program
  --print result

  let withLogging = addLogStatements parsedAst
  let fortran_seq = pprEfl withLogging
  writeFile "output/test.f90" fortran_seq
  --print withLogging
  return withLogging
