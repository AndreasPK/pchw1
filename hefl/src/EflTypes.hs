{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



{-# OPTIONS_GHC -O2 #-}



module EflTypes where

import Data.Text.Prettyprint.Doc

import qualified Data.Map as Map
import Data.Foldable
import Control.Monad
import Control.Monad.Identity
import GHC.Generics

import Data.Binary

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
    } deriving (Eq,Show,Ord, Generic)

data Type = INT | FLOAT deriving (Eq, Ord, Show, Generic)
data DimType = IsArr | IsScalar deriving (Eq, Ord, Show, Generic)

data ARR_SIZE
    = ARR_SIZE
    { aranks :: Int
    , ranges :: [(Int,Int)]
    }
    deriving (Eq, Ord, Show, Generic)

type Id = String

--Statements MUST be ordered descending
type StatementId = Int

type SymbolMap = Map.Map String Entry

-- | Loop depth
type LoopLevel = Int

data DepLevel = DepLevel [Ordering] | Independent deriving (Eq,Ord,Show, Generic)

data DependencyType = ANTI | OUT | TRUE | INPUT deriving (Eq, Ord, Show, Generic)

data Dependency a = Dependency
    { depStmts :: (Int,Int)
    , depType :: DependencyType
    , depLevel :: DepLevel
    , depVar :: Id
    , depExtra :: a
    } deriving (Eq, Ord, Show)

data DepEdge = DepEdge
  { edgeFrom :: StatementId
  , edgeTo :: StatementId
  , edges :: [(DependencyType, Int)]
  }
  deriving (Eq,Ord,Show, Generic)

instance Pretty DepEdge where
  pretty (DepEdge from to es) =
    "s" <> pretty from <> " -> " <> "s" <> pretty to <>
      "[label = \"" <> pprEdges es <> "\" ];"
    where
      pprType ANTI = "A"
      pprType OUT = "O"
      pprType TRUE = "T"
      pprType INPUT = "I"
      pprEdges [] = mempty
      pprEdges ((t,l):es) = pprType t <> pretty l <+> pprEdges es

type DepGraph = ([Int], [DepEdge])


data Var = Var { --NVAR
    entry :: Maybe Entry,
    indexExprs :: [Expr] }
    deriving (Eq,Show,Ord, Generic)

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
    | Write [Expr] --IR ONLY
    deriving (Eq, Ord, Show, Generic)

data Program
    = Program
    { name :: String
    , symbols :: [Entry]
    , stmts :: Statements }
    deriving (Eq, Ord, Show, Generic)

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
    deriving (Eq, Ord, Show, Generic)

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
    | VecExpr Expr Expr -- ^ IR ONLY, from:to
    deriving (Eq, Ord, Show, Generic)

foldExpr :: (b -> Expr -> b) -> b -> Expr -> b
foldExpr f z e@(FloatLit {}) = f z e
foldExpr f z e@(IntLit {}) = f z e
foldExpr f z e@(StringLit {}) = f z e
foldExpr f z e@(VarExpr (Var _ idxs)) = foldl' (foldExpr f) (f z e) idxs
foldExpr f z e@(ParensExpr e') = foldExpr f (f z e) e'
foldExpr f z e@(OpExpr _op e1 me2) =
    let z' = (foldExpr f (f z e) e1)
    in maybe z' (foldExpr f z') me2
foldExpr f z e@(VecExpr e1 e2) =
    let z' = (foldExpr f (f z e) e1)
    in foldExpr f z' e2

foldMapExpr :: (Expr -> [b]) -> Expr -> [b]
foldMapExpr f e = foldExpr (\l e -> l ++ f e) [] e

mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f e@(FloatLit {}) = f e
mapExpr f e@(IntLit {}) = f e
mapExpr f e@(StringLit {}) = f e
mapExpr f e@(VarExpr (Var entry idxs)) = f . VarExpr . Var entry $ map (mapExpr f) idxs
mapExpr f e@(ParensExpr e') = f (ParensExpr $ mapExpr f e')
mapExpr f e@(OpExpr op e1 e2) =
    let e1' = mapExpr f e1
        e2' = fmap (mapExpr f) e2
    in f (OpExpr op e1' e2')
mapExpr f e@(VecExpr e1 e2) =
    let e1' = mapExpr f e1
        e2' = mapExpr f e2
    in f (VecExpr e1' e2')

foldStatements :: (b -> Statement -> b) -> b -> Statement -> b
foldStatements f z stmt@(Assign {}) =
    f z stmt
foldStatements f z stmt@(If _ _ ts fs) =
    foldl' (foldStatements f) (f z stmt) (ts ++ fs)
foldStatements f z stmt@(For {body = bodyStmts}) =
    let z' = f z stmt
    in foldl' (foldStatements f) z' bodyStmts
foldStatements f z stmt@(Write {}) =
    f z stmt

isAssignment Assign {} = True
isAssignment _ = False

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


