module Main where

import Text.Megaparsec
import Text.Megaparsec.Char

import EflTypes
import IrParser
import System.Environment

import Data.Map as M
import Control.Monad.Trans.Reader
import Control.Monad

import Data.Either

--main :: IO ()
main = do
  s <- readFile "input/test1.ir"
  let readerM = runParserT program "Foo" s :: Reader SymbolTable (Either (ParseError Char String) Program)
  let result = runReader readerM M.empty :: (Either (ParseError Char String) Program)
  let parsedAst = fromRight (error "Failed to parse program") result :: Program
  print result


  return parsedAst
