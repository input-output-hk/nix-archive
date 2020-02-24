module Nix.Nar.Sha
  ( nixShaFile
  , nixShaGitRepoAtHash
  , sha256BS
  , sha256init
  , sha256LBS
  , sha256Update
  ) where

import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT, newExceptT)

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Crypto.Hash (Context, Digest, hashFinalize, hashInitWith, hashUpdate, hashUpdates, hashWith)
import           Crypto.Hash.Algorithms (SHA256 (..))

import           Nix.Nar.Base32 (Sha256 (..))
import qualified Nix.Nar.Base32 as Base32
import           Nix.Nar.Bits
import           Nix.Nar.Error
import           Nix.Nar.Git
import           Nix.Nar.Object

import           System.Directory (withCurrentDirectory)


nixShaFile :: FilePath -> IO BS.ByteString
nixShaFile fpath =
  rawSha256 . sha256LBS <$> LBS.readFile fpath

rawSha256 :: Digest SHA256 -> BS.ByteString
rawSha256 = Base32.encode . Sha256 . ByteArray.convert

sha256BS :: BS.ByteString -> Digest SHA256
sha256BS = hashWith SHA256

sha256LBS :: LBS.ByteString -> Digest SHA256
sha256LBS =
  hashFinalize
    . hashUpdates sha256init
    . LBS.toChunks

nixShaGitRepoAtHash :: FilePath -> GitHash -> IO (Either NarStatus BS.ByteString)
nixShaGitRepoAtHash gitdir ghashOrig =
  runExceptT $ do
    ghash <- firstExceptT NarGitError $ gitCheckHash gitdir ghashOrig
    newExceptT
      . withCurrentDirectory gitdir
      . runExceptT $ do
          nobjs <- bimapExceptT NarGitError gitToNarObjects $ gitRepoListAtHash ghash
          rawSha256 . sha256LBS <$> buildNarBits nobjs

sha256init :: Context SHA256
sha256init = hashInitWith SHA256

sha256Update :: Context SHA256 -> BS.ByteString -> Context SHA256
sha256Update = hashUpdate


