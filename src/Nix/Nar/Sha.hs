{-# LANGUAGE TypeApplications #-}
module Nix.Nar.Sha
  ( nixShaFile
  , sha256BS
  , sha256init
  , sha256LBS
  , sha256Update
  ) where

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Crypto.Hash (Context, Digest, hashFinalize, hashInitWith, hashUpdate, hashUpdates, hashWith)
import           Crypto.Hash.Algorithms (SHA256 (..))

import           Nix.Nar.Base32 (Sha256 (..))
import qualified Nix.Nar.Base32 as Base32


nixShaFile :: FilePath -> IO BS.ByteString
nixShaFile fpath =
  Base32.encode . Sha256 . ByteArray.convert . sha256LBS <$> LBS.readFile fpath

sha256BS :: BS.ByteString -> Digest SHA256
sha256BS = hashWith SHA256


sha256LBS :: LBS.ByteString -> Digest SHA256
sha256LBS =
  hashFinalize
    . hashUpdates sha256init
    . LBS.toChunks


sha256init :: Context SHA256
sha256init = hashInitWith SHA256

sha256Update :: Context SHA256 -> BS.ByteString -> Context SHA256
sha256Update ctx = hashUpdate ctx

