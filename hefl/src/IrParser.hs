{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module IrParser where

import Types

import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators




type IrParser = Parsec String ()

pid :: IrParser String
pid = do
    someTill char spaceChar
    eol
    return

symbolInfo :: IrParser Entry
symbolInfo = do
    name <- undefined
    undefined