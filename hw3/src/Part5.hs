{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Part5 (runParserM, processAction) where

import           Part1
import           Part2
import           Part3

import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.State.Class (MonadState (..), get)
import           Text.Megaparsec           (runParser)
import           Text.Megaparsec           (eof)

runEval :: ( MonadError ProcessingError      m
           , MonadState                 Env  m
           )
           => Expr -> m Integer

runEval expr = do
    env <- get
    case runReaderT (eval' expr) env of
        Left ee   -> throwError (ProcessingEvalError expr ee)
        Right val -> return val

runParserM :: ( MonadError ProcessingError m
               , MonadIO                    m
               )
           => Parser a -> String -> m a

runParserM parser str = case runParser parser "" str of
    Right expr -> return expr
    Left  err  -> throwError (ParsingError str err)


processAction :: ( MonadError ProcessingError      m
                 , MonadState                 Env  m
                 , MonadIO                         m
                 )
              => Action -> m ()

processAction (CreateVar name expr) = do
    val <- runEval expr
    createVar name val

processAction (ModifyVar name expr) = do
    val <- runEval expr
    modifyVar name val

processAction (PrintExpr expr) = do
    val <- runEval expr
    liftIO $ print val

processAction (ReadVar name) = do
    str <- liftIO getLine
    expr <- runParserM (exprParser <* eof) str
    val <- runEval expr
    insertVar name val

processAction (For name from to actions) = do
    evaledFrom <- runEval from
    evaledTo   <- runEval to
    processFor name evaledFrom evaledTo actions

processFor :: ( MonadError ProcessingError      m
              , MonadState                 Env  m
              , MonadIO                         m
              )
           => VarName -> Integer -> Integer -> [Action] -> m ()

processFor name cur to actions
    | cur > to  = return ()
    | otherwise = do
        modifyVar name cur
        mapM_ processAction actions
        processFor name (cur + 1) to actions
