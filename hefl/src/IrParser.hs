{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

{--
    Parser for EFL IR format, white space senstive!
-}
module IrParser where

import EflTypes as ET

import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Control.Monad

import Debug.Trace
import System.IO.Unsafe
import Control.Monad.IO.Class

import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Combinators
--import Control.Monad.Trans.State.Strict
import Control.Monad.State.Class
import Control.Monad.Reader

unsafeLog :: Monad m => String -> m ()
unsafeLog = return . unsafePerformIO . putStrLn

run :: IrParser a -> String -> Either (ParseError Char String) a
run p s = runReader (runParserT p "NoFile" s) (M.empty)

type IrParser = ParsecT String String (Reader SymbolMap)

getInfo :: (MonadReader (M.Map String Entry) m,
            MonadParsec e String m)
        => String -> m Entry
getInfo name = do
    tbl <- ask
    let mentry = M.lookup name tbl
    --unsafePerformIO $ do
    --    mapM_ print $ M.toList tbl
    --    return (return 1)
    maybe (error $ "No entry for id:" ++ name) return mentry

linespace :: IrParser ()
linespace = void $ takeWhileP Nothing (`elem` [' ', '\t'])

ptoken :: IrParser String
ptoken = someTill anyChar spaceChar <* space

pntoken :: IrParser Int
pntoken = read <$> try (someTill digitChar spaceChar <* space)

pid :: IrParser String
pid = pvarName

pvarName :: IrParser Id
pvarName = do
    c <- letterChar <|> char '_'
    cs <- manyTill (choice [alphaNumChar, char '\'', char '_']) spaceChar
    space
    return $ c : cs


ptype :: IrParser Type
ptype = do
    s <- string "INT" <|> string "FLOAT"
    t <- if s == "INT"
        then return INT
        else return FLOAT
    space1
    return t

pint :: IrParser Int
pint = pntoken


prange :: IrParser (Int,Int)
prange = pure (,) <*> pint <*> pint


symbolInfo :: IrParser Entry
symbolInfo = do
    name <- pvarName
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
    opArgCount <- (string "BINOP" >> space1 >> return 2) <|>
                   (string "UNOP" >> space1 >> return 1)
    let operators
            = [ ("=="     , ET.EQ)
            ,   ("<>"     , NE   )
            ,   (">"      , ET.GT)
            ,   (">="     , GE   )
            ,   ("<"      , ET.LT)
            ,   ("<="     , LE   )
            ,   ("+"      , PLUS )
            ,   ("-"      , MINUS)
            ,   ("/"      , DIV  )
            ,   ("*"      , MULT )
            ,   (".and."  , AND  )
            ,   (".or."   , OR   )
            ,   (".not."  , NOT  )
            ]
    let opParsers = map (\(s,op) -> try (string s >> space1 >> return op))
                        operators
    op <- choice (opParsers ++ [error "Unknown operator"])
    return (opArgCount, op)

popExpr :: IrParser Expr
popExpr = do
    (argCount,op) <- pop
    arg1 <- pexpr
    arg2 <- if argCount == 2
            then Just <$> pexpr
            else return Nothing
    return $ OpExpr op arg1 arg2

pfloatExpr :: IrParser Expr
pfloatExpr = do
    string "FLOAT" >> space1
    ds <- ptoken

    --does it parse as float
    seq (read ds :: Float) (return ())
    return $
        FloatLit ds

pintExpr :: IrParser Expr
pintExpr = do
    string "INT" >> space1
    i <- pint
    return $
        IntLit i

--"NOOP" expressions - just a way to express parens
pparensExpr :: IrParser Expr
pparensExpr = ParensExpr <$> pexpr

pexpr :: IrParser Expr
pexpr = do
    string "EXPR" >> space1
    (   try pintExpr <|>
        try pvarExpr <|>
        try popExpr  <|>
        try pfloatExpr <|>
        try pparensExpr )

pvarExpr :: IrParser Expr
pvarExpr = VarExpr <$> pvar

pvar :: IrParser Var
pvar = do
    string "VAR" <* space1
    varEntry <- pentry
    exprs <- optional (try pexprList)
    return $ (Var (Just varEntry) (fromMaybe [] exprs))

pentry :: IrParser Entry
pentry = do
    string "ENTRY" <* space1
    varName <- pvarName
    info <- getInfo varName
    return info


pexprList :: IrParser [Expr]
pexprList = do
    string "EXPRLIST" >> space1
    exprs <- many (try pexpr)
    string "/EXPRLIST" >> space
    return exprs

pstmtNumber :: IrParser Int
pstmtNumber = char '@' >> (pint <* space)

pfor :: IrParser Statement
pfor = do
    string "FOR" >> space1
    n <- pstmtNumber
    loopvar <- pentry
    initial <- pexpr
    final <- pexpr
    step <- fromMaybe (IntLit 1) <$> optional (try pexpr)
    body <- pstmtList
    return $
        For n loopvar initial final step body

passign :: IrParser Statement
passign = do
    string "ASSIGN" >> space1
    n <- pstmtNumber
    var <- pvar
    rhs <- pexpr
    return $
        Assign n var rhs

pif :: IrParser Statement
pif = do
    string "IF" >> space1
    n <- pstmtNumber
    cond <- pexpr
    string "THEN" >> space1
    thenBody <- pstmtList
    elseBody <- pelse <|> return []
    return $
        If n cond thenBody elseBody

pelse :: IrParser Statements
pelse = do
    string "ELSE" >> space1
    pstmtList

pstmt :: IrParser Statement
pstmt = do
    choice $ map try [passign, pfor, pif]

pstmtList :: IrParser [Statement]
pstmtList = do
    space
    string "STMTLIST" >> space1
    stmts <- many (try pstmt)
    string "/STMTLIST" >> space1
    return stmts

mkSymbolMap :: [Entry] -> M.Map String Entry
mkSymbolMap =
    foldl' (\m e -> M.insert (varName e) e m) M.empty

program :: IrParser Program
program = do
    name <- ptoken
    symbols <- many (try symbolInfo)
    let symTbl = mkSymbolMap symbols
    stmts <- local (const symTbl) pstmtList
    eof
    return $
        Program name symbols stmts
