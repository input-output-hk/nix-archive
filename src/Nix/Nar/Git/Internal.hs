{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module Nix.Nar.Git.Internal
  ( GitError (..)
  , gitBinary
  , gitProcess
  , readProcess
  , renderGitError
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           System.Directory (getCurrentDirectory)
import           System.Exit (ExitCode (..))
import qualified System.Process.ByteString as Process


data GitError
  = GEGitDirMissing
  | GEParsError String
  | GEProcessError ByteString
  deriving (Eq, Show)


gitBinary :: String
gitBinary = "git"

gitProcess :: [String] -> ExceptT GitError IO ByteString
gitProcess = readProcess gitBinary

-- | The 'System.Process.readProcess' function does not affect 'stderr'. This version
-- captures 'stderr' pipes it to the output along with 'stdout.
readProcess
    :: FilePath                         -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                         -- ^ any arguments
    -> ExceptT GitError IO ByteString   -- ^ output
readProcess cmd args = do
  -- liftIO $  getCurrentDirectory >>= \ cwd -> print (cwd, cmd : args)
  (code, out, err) <- liftIO $ Process.readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> pure $ err <> out
    ExitFailure _ -> do
      cwd <- liftIO getCurrentDirectory
      left $ GEProcessError (mconcat [BS.pack cwd, ":\n  ", err, "\n  ", out])

renderGitError :: GitError -> Text
renderGitError ge =
  case ge of
    GEGitDirMissing -> "Provided directory does not contain a .git subdirectory."
    GEParsError err -> mconcat [ "Git parse error: ", Text.pack err ]
    GEProcessError bs -> mconcat ["Nix.Nar.Git.Internal.readProcess", ": ",  Text.decodeUtf8 bs ]


