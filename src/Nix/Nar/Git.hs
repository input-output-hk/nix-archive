{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar.Git
  ( GitError (..)
  , GitHash (..)
  , GitObject (..)
  , GitObjectType (..)
  , GitSubModulePath (..)
  , gitAssertDir
  , gitBinary
  , gitCatFile
  , gitCheckHash
  , gitHeadHash
  , gitRepoListAtHash
  , gitProcess
  , isGitBlob
  , renderGitError
  ) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, left, newExceptT, runExceptT)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.Bifunctor (first)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as Char

import           GHC.Generics (Generic)

import           Nix.Nar.Git.Internal

import           Quiet (Quiet (..))

import           System.Directory (doesDirectoryExist, withCurrentDirectory)

newtype GitHash
  -- A git hash, but can also be the string "HEAD"
  = GitHash { unGitHash :: ByteString }
  deriving (Eq, Generic)
  deriving (Read, Show) via (Quiet GitHash)

data GitObjectType
  = GOTBlob
  | GOTCommit
  deriving (Eq, Generic, Read, Show)

data GitObject = GitObject
  { goPerms :: Word
  , goType :: GitObjectType
  , goHash :: GitHash
  , goName :: FilePath
  } deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet GitObject)

newtype GitSubModulePath = GitSubModulePath
  { unGitSubModulePath :: FilePath
  } deriving (Eq, Generic)
    deriving (Read, Show) via (Quiet GitSubModulePath)

gitAssertDir :: ExceptT GitError IO ()
gitAssertDir = do
  exists <- liftIO $ doesDirectoryExist ".git"
  unless exists $
    left GEGitDirMissing

gitCatFile :: GitHash -> ExceptT GitError IO ByteString
gitCatFile (GitHash hash) =
  gitProcess [ "cat-file", "-p", BS.unpack hash ]

gitCheckHash :: FilePath -> GitHash -> ExceptT GitError IO GitHash
gitCheckHash dir ghash =
  if BS.all Char.isHexDigit (unGitHash ghash)
    then pure ghash
    else newExceptT .
           withCurrentDirectory dir $
             runExceptT gitHeadHash

gitHeadHash :: ExceptT GitError IO GitHash
gitHeadHash =
  GitHash
    . BS.takeWhile Char.isHexDigit
    <$> gitProcess [ "rev-parse", "HEAD" ]

gitRepoListAtHash :: GitHash -> ExceptT GitError IO [GitObject]
gitRepoListAtHash (GitHash hash) = do
  bs <- gitProcess [ "ls-tree", "--full-tree", "-r", BS.unpack hash ]
  -- 'ls-tree' seems to return file objects sorted by their names, and that seems to
  -- be what is needed for nar files.
  hoistEither $ first GEParsError (Atto.parseOnly lsParser bs)

isGitBlob :: GitObject -> Bool
isGitBlob go =
  goType go == GOTBlob

-- -------------------------------------------------------------------------------------------------

lsParser :: Parser [GitObject]
lsParser =
   Atto.many' pGitObject <* Atto.endOfInput

pGitObject :: Parser GitObject
pGitObject =
  GitObject
    <$> Atto.decimal
    <*> (pSkipHSpace *> pBlobOrCommit)
    <*> (pSkipHSpace *> pGitHash)
    <*> (pSkipHSpace *> pFilename <* Atto.endOfLine)

pBlobOrCommit :: Parser GitObjectType
pBlobOrCommit =
  Atto.choice
    [ Atto.string "blob" *> pure GOTBlob
    , Atto.string "commit" *> pure GOTCommit
    ]

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
