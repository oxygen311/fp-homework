{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Part1
    (Expr(..), EvaluatingError(..), VariableError(..), ProcessingError(..),  eval, Env, VarName, eval', Action(..)) where

import           Control.Monad              (liftM2, return)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Reader.Class (MonadReader (..), asks)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as M
import           Data.Void                  (Void)
import           Text.Megaparsec.Error      (ParseError (..))

type VarName = String
type Env = M.Map VarName Integer

data Expr = Val Integer
          | Var VarName
          | Sum Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Let VarName Expr Expr

instance Show Expr where
    show (Val val) = show val
    show (Var nm)  = nm
    show (Sum l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
    show (Sub l r) = "(" ++ show l ++ " - " ++ show r ++ ")"
    show (Mul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"
    show (Div l r) = "(" ++ show l ++ " / " ++ show r ++ ")"
    show (Let name l r) = "(let" ++ name ++ " = " ++ show l ++ "in" ++ show r ++ ")"

data EvaluatingError = VariableNotInScope | DivideByZero
    deriving Show

data VariableError = VariableNotDefined | VariableAlreadyDefined
    deriving Show

data Action = CreateVar VarName Expr | ModifyVar VarName Expr | PrintExpr Expr | ReadVar VarName | For VarName Expr Expr [Action]

instance Show Action where
    show (CreateVar name expr) = "mut " ++ name ++ " = " ++ show expr
    show (ModifyVar name expr) =           name ++ " = " ++ show expr
    show (PrintExpr expr)      = "< " ++ show expr
    show (ReadVar name)        = "> " ++ name
    show (For name from to actions) = "for " ++ name ++ " in " ++ show from ++ ".." ++ show to ++ "{" ++ intercalate "; " (map show actions) ++ "}"

data ProcessingError = ProcessingVarError VarName VariableError | ProcessingEvalError Expr EvaluatingError | ParsingError String (ParseError Char Void)

instance Show ProcessingError where
    show (ProcessingVarError name varErr)   = "Error \"" ++ show varErr ++ "\" while processing var \"" ++ name ++ "\""
    show (ProcessingEvalError expr evalErr) = "Evaluating error \"" ++ show evalErr ++ "\" while processing expr \"" ++ show expr ++ "\""
    show (ParsingError string parErr)       = "Paring error \"" ++ show parErr ++ "\" while parsing string \"" ++ string ++ "\""

eval :: ( MonadError   EvaluatingError      m
        , MonadReader                  Env  m
        )
     => Expr -> m Integer

eval (Val x) = return x

eval (Let v l r) = do
    evaledL <- eval l
    local (M.insert v evaledL) (eval r)

eval (Var x) = do
    res <- asks (M.lookup x)
    case res of
        Nothing  -> throwError VariableNotInScope
        Just val -> return val

eval (Sum l r) = liftM2 (+) (eval l) (eval r)
eval (Sub l r) = liftM2 (-) (eval l) (eval r)
eval (Mul l r) = liftM2 (*) (eval l) (eval r)

eval (Div l r) = do
    evaledR <- eval r
    if evaledR == 0
        then throwError DivideByZero
        else liftM2 div (eval l) (return evaledR)

eval' :: Expr -> ReaderT Env (Either EvaluatingError) Integer
eval' = eval
