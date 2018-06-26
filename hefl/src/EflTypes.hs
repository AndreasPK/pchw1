{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}


module EflTypes where

import qualified Data.Map as Map
import Data.Foldable

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

getVarName :: Var -> Id
getVarName (Var (Just e) _) = varName e

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
    | Write [Expr]
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
    = FloatLit String
    | IntLit Int
    | StringLit String
    | VarExpr Var
    | ParensExpr Expr
    | OpExpr
    { exprOp :: Op
    , arg1 :: Expr
    , arg2 :: Maybe Expr }
    deriving (Eq, Ord, Show)

foldExpr :: (b -> Expr -> b) -> b -> Expr -> b
foldExpr f z e@(FloatLit {}) = f z e
foldExpr f z e@(IntLit {}) = f z e
foldExpr f z e@(StringLit {}) = f z e
foldExpr f z e@(VarExpr (Var _ idxs)) = foldl' f (f z e) idxs
foldExpr f z e@(ParensExpr e') = f (f z e) e'
foldExpr f z e@(OpExpr _op e1 me2) =
    let z' = (f (f z e) e1)
    in maybe z' (f z') me2


foldStatements :: (b -> Statement -> b) -> b -> Statement -> b
foldStatements f z stmt@(Assign {}) =
    f z stmt
foldStatements f z stmt@(If _ _ ts fs) =
    foldl' f (f z stmt) (ts ++ fs)
foldStatements f z stmt@(For {body = bodyStmts}) =
    foldl' f (f z stmt) bodyStmts
foldStatements f z stmt@(Write {}) =
    f z stmt


