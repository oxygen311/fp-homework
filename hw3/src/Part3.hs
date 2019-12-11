{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Part3 (createVar, modifyVar, insertVar) where

import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.State.Class  (MonadState (..), gets, modify)
import qualified Data.Map.Strict            as M
import           Part1

createVar :: ( MonadError   ProcessingError      m
             , MonadState                   Env  m
             )
          => VarName -> Integer -> m ()

createVar name val = do
    mem <- gets (M.member name)
    if mem
        then throwError (ProcessingVarError name VariableAlreadyDefined)
        else modify (M.insert name val)

modifyVar :: ( MonadError  ProcessingError      m
             , MonadState                  Env  m
             )
          => VarName -> Integer -> m ()

modifyVar name val = do
    mem <- gets (M.member name)
    if mem
        then modify (M.insert name val)
        else throwError (ProcessingVarError name VariableNotDefined)

insertVar :: (MonadState Env  m)
          => VarName -> Integer -> m ()

insertVar name val = modify (M.insert name val)