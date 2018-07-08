{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}



module EflTypes where

import qualified Data.Map as Map
import Data.Foldable
import Control.Monad
import Control.Monad.Identity

import Debug.Trace

-- | A version of 'concatMap' that works with a monadic predicate.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

-- Symbol description
data Entry
    = ENTRY
    { varName :: Id,
      varType :: Type,
      varDim :: DimType,
      varArrSize :: (Maybe ARR_SIZE)
    } deriving (Eq,Show,Ord)

data Type = INT | FLOAT deriving (Eq, Ord, Show)
data DimType = IsArr | IsScalar deriving (Eq, Ord, Show)

data ARR_SIZE
    = ARR_SIZE
    { aranks :: Int
    , ranges :: [(Int,Int)]
    }
    deriving (Eq, Ord, Show)

type Id = String

--Statements MUST be ordered descending
type StatementId = Int

type SymbolMap = Map.Map String Entry

-- | Loop depth
type LoopLevel = Int

data DepLevel = DepLevel [Ordering] | Independent deriving (Eq,Ord,Show)

data DependencyType = ANTI | OUT | TRUE | INPUT deriving (Eq, Ord, Show)

data Dependency = Dependency
    { depStmts :: (Int,Int)
    , depType :: DependencyType
    , depLevel :: DepLevel
    , depVar :: Id
    } deriving (Eq, Ord, Show)


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
foldExpr f z e@(VarExpr (Var _ idxs)) = foldl' (foldExpr f) (f z e) idxs
foldExpr f z e@(ParensExpr e') = foldExpr f (f z e) e'
foldExpr f z e@(OpExpr _op e1 me2) =
    let z' = (foldExpr f (f z e) e1)
    in maybe z' (foldExpr f z') me2

foldMapExpr :: (Expr -> [b]) -> Expr -> [b]
foldMapExpr f e = foldExpr (\l e -> l ++ f e) [] e

foldStatements :: (b -> Statement -> b) -> b -> Statement -> b
foldStatements f z stmt@(Assign {}) =
    f z stmt
foldStatements f z stmt@(If _ _ ts fs) =
    foldl' f (f z stmt) (ts ++ fs)
foldStatements f z stmt@(For {body = bodyStmts}) =
    foldl' f (f z stmt) bodyStmts
foldStatements f z stmt@(Write {}) =
    f z stmt

--Replace occurrences of statemens with a list of statements
updateStatementsM :: Monad m => (Statement -> m Statements) -> Statement -> m Statements
updateStatementsM f stmt@Assign{} =
    f stmt
updateStatementsM f stmt@If{..} = do
    thenStmts <- concatMapM (updateStatementsM f) thenBlock
    elseStmts <- concatMapM (updateStatementsM f) elseBlock
    f (stmt{thenBlock = thenStmts, elseBlock = elseStmts})
updateStatementsM f stmt@For{..} = do
    bodyStmts <- concatMapM (updateStatementsM f) body
    f stmt{body = bodyStmts}
updateStatementsM f stmt@Write{} =
    f stmt

updateStatements :: (Statement -> Statements) -> Statement -> Statements
updateStatements f s =
    let Identity res = (updateStatementsM (return . f)) s
    in res


