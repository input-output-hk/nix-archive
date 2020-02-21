{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Nix.Git
  ( GitError (..)
  , GitHash (..)
  , GitObject (..)
  , gitAssertDir
  , gitCatFile
  , gitHeadHash
  , gitListHash
  , renderGitError
  ) where

import           Control.Applicative ((<*))
import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, left)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor (first)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

import           GHC.Generics (Generic)

import           Nix.Git.Internal

import           Quiet (Quiet (..))

import           System.Directory (doesDirectoryExist)

data GitError
  = GEGitDirMissing
  | GEParsError String
  deriving (Eq, Show)

newtype GitHash
  -- A git hash, but can also be the string "HEAD"
  = GitHash { unGitHash :: ByteString }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet GitHash)

data GitObject = GitObject
  { goPerms :: Word
  , goHash :: GitHash
  , goName :: FilePath
  } deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet GitObject)

gitAssertDir :: ExceptT GitError IO ()
gitAssertDir = do
  exists <- liftIO $ doesDirectoryExist ".git"
  unless exists $
    left GEGitDirMissing
  pure ()

gitCatFile :: GitHash -> ExceptT GitError IO ByteString
gitCatFile (GitHash hash) =
  liftIO $ gitProcess [ "cat-file", "-p", BS.unpack hash ]

gitHeadHash :: ExceptT GitError IO GitHash
gitHeadHash =
  GitHash
    . BS.takeWhile (Char.isHexDigit)
    <$> liftIO (gitProcess [ "rev-parse", "HEAD" ])

gitListHash :: GitHash -> ExceptT GitError IO [GitObject]
gitListHash (GitHash hash) = do
  bs <- liftIO $ gitProcess [ "ls-tree", "--full-tree", "-r", BS.unpack hash ]
  -- 'ls-tree' seems to return file objects sorted by their names, and that seems to
  -- be what is needed for nar files.
  hoistEither $ first GEParsError (Atto.parseOnly lsParser bs)

renderGitError :: GitError -> Text
renderGitError ge =
  case ge of
    GEGitDirMissing -> "Provided directory does not contain a .git subdirectory."
    GEParsError err -> mconcat [ "Git parse error: ", Text.pack err ]

-- -------------------------------------------------------------------------------------------------

lsParser :: Parser [GitObject]
lsParser =
   Atto.many' pGitObject <* Atto.endOfInput

pGitObject :: Parser GitObject
pGitObject =
  GitObject
    <$> Atto.decimal
    <*> (pSkipHSpace *> Atto.string "blob" *> pSkipHSpace *> pGitHash)
    <*> (pSkipHSpace *> pFilename <* Atto.endOfLine)

pFilename :: Parser String
pFilename =
  -- File name may contain spaces.
  BS.unpack <$> Atto.takeWhile1 (/= '\n')

pGitHash :: Parser GitHash
pGitHash =
  GitHash <$> Atto.takeWhile1 Char.isHexDigit

pSkipHSpace :: Parser ()
pSkipHSpace =
  Atto.skipWhile (`elem` [' ', '\t'])
