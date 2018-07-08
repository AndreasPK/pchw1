{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EflInterpreter where

import EflTypes as ET
import Control.Monad

import Text.Printf
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Foldable

import qualified Data.Map as Map



