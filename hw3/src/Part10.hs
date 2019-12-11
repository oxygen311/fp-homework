{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Part10 (parseAndProcess) where

import           Part1
import           Part4
import           Part5

import           Control.Monad                  ((>=>))
import           Control.Monad.Except           (ExceptT (..))
import           Control.Monad.Trans.State.Lazy (StateT (..))

parseAndProcess :: [String] -> ExceptT ProcessingError (StateT Env IO) ()
parseAndProcess = mapM_ $ runParserM actionParser >=> processAction