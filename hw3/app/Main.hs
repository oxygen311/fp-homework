module Main where

import           Part10

import           Control.Monad                  (void)
import           Control.Monad.Error.Class      (MonadError (..))
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Except     (runExceptT)
import           Control.Monad.Trans.State.Lazy (runStateT)
import qualified Data.Map.Strict                as M
import           System.Environment             (getArgs)

main :: IO ()
main = do
    args <- getArgs
    file <- readFile (head args)
    _ <- runStateT (runExceptT (parseAndProcess (lines file)
                    `catchError` (void . liftIO . print))) M.empty
    return ()
