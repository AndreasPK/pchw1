module Main where

import Text.Megaparsec
import Text.Megaparsec.Char

import EflTypes
import IrParser
import System.Environment

main :: IO ()
main = do
  s <- readFile "input/test1.ir"
  print $ runParser program "Foo" s
