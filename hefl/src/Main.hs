module Main where

import Text.Megaparsec
import Text.Megaparsec.Char

import EflTypes
import IrParser
import EflLog
import LogParser
import Deps
import IrPrinter
import System.Environment

import qualified Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad

import Data.Either

getLogEntries = do
  addLoopInfo . parseLogs <$> readFile "output\\test.f90.log"

getInsts = 
  mkStmtInst <$> getLogEntries

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
