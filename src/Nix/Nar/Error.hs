{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar.Error
  ( NarStatus (..)
  , renderNarStatus
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Nix.Nar.Git

data NarStatus
  = NarSuccess FilePath
  | NarNotDir FilePath
  | NarGitError GitError
  | NarReadFail FilePath String
  deriving Show

renderNarStatus :: NarStatus -> Text
renderNarStatus ns =
  case ns of
    NarSuccess fp -> mconcat [ "Generated nar file: ", Text.pack fp ]
    NarNotDir fp -> mconcat [ "Provided filepath ", Text.pack fp, " is not a directory" ]
    NarGitError ge -> renderGitError ge
    NarReadFail fp err -> mconcat [ "Reading file ", Text.pack fp, " failed: ", Text.pack err ]
