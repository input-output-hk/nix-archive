{-# LANGUAGE OverloadedStrings #-}
module Nix.Git.Internal
  ( gitBinary
  , gitProcess
  , readProcess
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           System.Exit (ExitCode (..))
import qualified System.Process.ByteString as Process

gitBinary :: String
gitBinary = "git"


gitProcess :: [String] -> IO ByteString
gitProcess args =
  readProcess gitBinary args

-- | The 'System.Process.readProcess' function does not affect 'stderr'. This version
-- captures 'stderr' pipes it to the output along with 'stdout.
readProcess
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> IO ByteString            -- ^ output
readProcess cmd args = do
  (code, out, err) <- Process.readProcessWithExitCode cmd args ""
  case code of
    ExitSuccess -> pure $ err <> out
    ExitFailure _ -> error $ "Nix.Git.Internal.readProcess: " ++ BS.unpack (err <> out)
