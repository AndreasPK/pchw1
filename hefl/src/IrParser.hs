{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{--
    Parser for EFL IR format, white space senstive!
-}
module IrParser where

import EflTypes as ET

import Data.Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Control.Monad
import Debug.Trace
import System.IO.Unsafe

unsafeLog :: Monad m => String -> m ()
unsafeLog = return . unsafePerformIO . putStrLn


type IrParser = Parsec String String  

linespace :: IrParser ()
linespace = void $ takeWhileP Nothing (`elem` [' ', '\t'])

ptoken :: IrParser String
ptoken = someTill anyChar spaceChar <* linespace

pntoken :: IrParser Int
pntoken = read <$> someTill digitChar spaceChar <* linespace

pid :: IrParser String
pid = ptoken

ptype :: IrParser Type
ptype = do
    s <- string "INT" <|> string "FLOAT"
    t <- if s == "INT"
        then return INT
        else return FLOAT
    linespace
    return t

pint :: IrParser Int
pint = pntoken

pfloat :: IrParser Double
pfloat = read <$> ptoken

prange :: IrParser (Int,Int)
prange = pure (,) <*> pint <*> pint


symbolInfo :: IrParser Entry
symbolInfo = do
    name <- pid
    t <- ptype
    dim <- (Just <$> try pint) <|> return Nothing
    case dim of
        Nothing -> return $
            ENTRY name t IsScalar Nothing
        Just d -> do
            ranges <- replicateM d prange
            return $
                ENTRY name t IsArr (Just $
                    ARR_SIZE d ranges)

pop :: IrParser (Int,Op)
pop = do
    opTypeToken <- ptoken
    let opArgCount 
            | opTypeToken == "BINOP" 
            = 2
            | otherwise 
            = 1
    s <- ptoken
    let op = case s of
            "==" -> ET.EQ
            "<>" -> NE
            ">" -> ET.GT
            ">=" -> GE
            "<" -> ET.LT
            "<=" -> LE
            "+" -> PLUS
            "-" -> MINUS
            "/" -> DIV
            "*" -> MULT
            ".and." -> AND
            ".or." -> OR
            ".not." -> NOT
            s -> error $ "Unknown operator " ++ s
    return (opArgCount, op)

pexpr :: IrParser Expr
pexpr = do
    string "EXPR"
    space
    (
        try (string "INT" >> space >> IntLit <$> pint) <|>
        try (pvarExpr) <|>
        try (popExpr) <|>
        try (string "FLOAT" >> space >> FloatLit <$> pfloat)
        )

pvarExpr :: IrParser Expr
pvarExpr = do
    string "VAR" <* space
    string "ENTRY" <* space
    exprs <- optional (try pexprList)
    
    id <- ptoken

pexprList :: IrParser [Expr]
pexprList = do
    string "EXPRLIST"
    exprs <- many (try pexpr)
    string "/EXPRLIST"
    return [exprs]
popExpr = undefined
        


program :: IrParser Program
program = do
    name <- ptoken
    symbols <- many (try symbolInfo)
    traceM $ show symbols 
    many anyChar
    fail "Foo"