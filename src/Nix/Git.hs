{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Nix.Git where

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

import           GHC.Generics (Generic)

import           Nix.Git.Internal

import           Quiet (Quiet (..))

import           System.Directory (doesDirectoryExist)

data GitError
  = GEGitDirMissing
  | GEParsError String
  deriving (Eq, Show)

newtype GitHash
  = GitHash { unGitHash :: ByteString }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet GitHash)

data GitObject = GitObject
  { goPerms :: Word
  , goHash :: GitHash
  , goName :: FilePath
  } deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet GitObject)

gitListObjects :: GitHash -> ExceptT GitError IO [GitObject]
gitListObjects (GitHash hash) = do
  exists <- liftIO $ doesDirectoryExist ".git"
  unless exists $
    left GEGitDirMissing
  bs <- liftIO $ gitProcess [ "ls-tree", "--full-tree", "-r", BS.unpack hash ]
  hoistEither $ first GEParsError (Atto.parseOnly lsParser bs)


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
