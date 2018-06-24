{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}


module EflTypes where

import qualified Data.Map as Map

-- Symbol description
data Entry
    = ENTRY
    { varName :: Id,
      varType :: Type,
      varDim :: DimType,
      varArrSize :: (Maybe ARR_SIZE)
    } deriving (Eq,Show,Ord)

data Type = INT | FLOAT deriving (Eq, Ord, Show)
data Dependency = ANTI | OUT | TRUE deriving (Eq, Ord, Show)
data DimType = IsArr | IsScalar deriving (Eq, Ord, Show)

data ARR_SIZE
    = ARR_SIZE
    { aranks :: Int
    , ranges :: [(Int,Int)]
    }
    deriving (Eq, Ord, Show)

type Id = String

type SymbolMap = Map.Map String Entry


data Var = Var { --NVAR
    entry :: Maybe Entry,
    indexExprs :: [Expr] }
    deriving (Eq,Show,Ord)

type Statements = [Statement]
data Statement
    = Assign
        { stmtNr :: Int
        , assignTo :: Var
        , rhs :: Expr
        }
    | If
        { stmtNr :: Int
        , cond :: Expr
        , thenBlock :: Statements
        , elseBlock :: Statements }
    | For
        { stmtNr :: Int
        , loopvar :: Entry
        , lb :: Expr
        , ub :: Expr
        , step :: Expr
        , body :: Statements
        }
    deriving (Eq, Ord, Show)

data Program
    = Program
    { name :: String
    , symbols :: [Entry]
    , stmts :: Statements }
    deriving (Eq, Ord, Show)

data Op
    = PLUS
    | MINUS
    | MULT
    | DIV
    | EQ
    | LE
    | LT
    | GE
    | GT
    | NE
    | AND
    | OR
    | NOT
    deriving (Eq, Ord, Show)

data Expr
    = FloatLit Double
    | IntLit Int
    | VarExpr Var
    | OpExpr
    { exprOp :: Op
    , arg1 :: Expr
    , arg2 :: Maybe Expr }
    deriving (Eq, Ord, Show)
