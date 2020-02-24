{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar
  ( archive
  , buildNarBits
  , renderNarStatus
  , module X
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Extra (left, firstExceptT, newExceptT)

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

import           Nix.Git
import           Nix.Nar.Bits
import           Nix.Nar.Error
import           Nix.Nar.Object
import           Nix.Nar.Sha as X

import           System.Directory (doesDirectoryExist, setCurrentDirectory, withCurrentDirectory)
import           System.FilePath (takeFileName)
import           System.IO (Handle, IOMode (..), withFile)

archive :: FilePath -> GitHash -> IO NarStatus
archive dir ghash = do
  res <- runExceptT $ archiveExcept dir ghash
  case res of
    Left err -> pure err
    Right fp -> pure $ NarSuccess fp

-- -------------------------------------------------------------------------------------------------

archiveExcept :: FilePath -> GitHash -> ExceptT NarStatus IO FilePath
archiveExcept dir gHashOrig = do
  isDir <- liftIO $ doesDirectoryExist dir
  unless isDir $
    left $ NarNotDir dir
  ghash <- gitCheckHash dir gHashOrig
  let fpath = takeFileName dir ++ "-" ++ BS.unpack (unGitHash ghash) ++ ".nar"
  newExceptT .
    withFile fpath WriteMode $ \ handle -> do
      setCurrentDirectory dir
      runExceptT $ do
        firstExceptT NarGitError gitAssertDir
        objs <- firstExceptT NarGitError (gitListHash ghash)
        let nobjs = gitToNarObjects objs
        unless True .
          -- Debug
          liftIO $ do
            putStrLn ""
            printNarObjects nobjs
            putStrLn ""
        writeNarTopLevel handle nobjs
  pure fpath

gitCheckHash :: FilePath -> GitHash -> ExceptT NarStatus IO GitHash
gitCheckHash dir ghash =
  if BS.all Char.isHexDigit (unGitHash ghash)
    then pure ghash
    else newExceptT .
           withCurrentDirectory dir .
             runExceptT $
               firstExceptT NarGitError gitHeadHash


writeNarTopLevel :: Handle -> [NarObject] -> ExceptT NarStatus IO ()
writeNarTopLevel h objs = do
  lbs <- buildNarBits objs
  liftIO $ LBS.hPutStr h lbs
