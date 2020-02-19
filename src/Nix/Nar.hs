module Nix.Nar where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Except.Extra (left)

import System.Directory (doesDirectoryExist)
import System.FilePath (takeFileName)


data NarStatus
  = NarSuccess FilePath
  | NarNotDir FilePath

archive :: FilePath -> IO NarStatus
archive dir = do
  res <- runExceptT $ archiveExcept dir
  case res of
    Left err -> pure err
    Right () -> pure $ NarSuccess (takeFileName dir ++ ".nar")


archiveExcept :: FilePath -> ExceptT NarStatus IO ()
archiveExcept dir = do
  isDir <- liftIO $ doesDirectoryExist dir
  unless isDir $
    left $ NarNotDir dir
  pure ()
