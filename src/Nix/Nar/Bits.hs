{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar.Bits
  ( buildNarBits
  , renderNarStatus
  ) where

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, newExceptT, runExceptT)

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Nix.Nar.Git
import           Nix.Nar.Error
import           Nix.Nar.Object

import           System.Directory (withCurrentDirectory)


buildNarBits :: [NarObject] -> ExceptT NarStatus IO LBS.ByteString
buildNarBits objs = do
    middle <- concat <$> mapM buildBitsNarObject objs
    pure $ LBS.fromChunks (header ++ middle ++ [nsCloseParen])
  where
    header :: [BS.ByteString]
    header =
      [ narHeader
      , nsOpenParen
      , nsType
      , nsDirectory
      ]

-- -------------------------------------------------------------------------------------------------

buildBitsNarObject :: NarObject -> ExceptT NarStatus IO [BS.ByteString]
buildBitsNarObject obj =
  case obj of
    NarFile name ghash nft -> buildBitsNarFile name ghash nft
    NarDir name objs -> buildBitsNarDir name objs
    NarSubModule name ghash -> buildBitsNarSubModule (GitSubModulePath name) ghash

buildBitsNarDir :: String -> [NarObject] -> ExceptT NarStatus IO [BS.ByteString]
buildBitsNarDir name objs = do
    middle <- concat <$> mapM buildBitsNarObject objs
    pure $ header ++ middle ++ tailer
  where
    header :: [BS.ByteString]
    header =
      [ nsEntry
      , nsOpenParen
      , nsName
      , narString (BS.pack name)
      , nsNode
      , nsOpenParen
      , nsType
      , nsDirectory
      ]
    tailer :: [BS.ByteString]
    tailer =
      [ nsCloseParen
      , nsCloseParen
      ]

buildBitsNarFile :: FilePath -> GitHash -> NarFileType -> ExceptT NarStatus IO [BS.ByteString]
buildBitsNarFile fpath ghash nft = do
  bs <- firstExceptT NarGitError $ gitCatFile ghash
  pure $
    [ nsEntry
    , nsOpenParen
    , nsName
    , narString (BS.pack fpath)
    , nsNode
    , nsOpenParen
    , nsType
    , narString (if nft == NarSymlink then "symlink" else "regular")
    ]
    ++ exeMarker
    ++
    [ narString (if nft == NarSymlink then "target" else "contents")
    , narString bs
    , nsCloseParen
    , nsCloseParen
    ]
  where
    exeMarker :: [BS.ByteString]
    exeMarker =
      if nft == NarExecuatble
        then [ narString "executable", narString "" ]
        else []

buildBitsNarSubModule :: GitSubModulePath -> GitHash -> ExceptT NarStatus IO [BS.ByteString]
buildBitsNarSubModule (GitSubModulePath smpath) ghash =
  newExceptT
    . withCurrentDirectory (".git/modules/" ++ smpath)
    . runExceptT $ do
        gobjs <- firstExceptT NarGitError $ gitRepoListAtHash ghash
        concat <$> mapM buildBitsNarObject (gitToNarObjects gobjs)

-- -------------------------------------------------------------------------------------------------

narHeader :: BS.ByteString
narHeader = "\x0d\x00\x00\x00\x00\x00\x00\x00nix-archive-1\x00\x00\x00"

nsCloseParen :: BS.ByteString
nsCloseParen = narString ")"

nsDirectory :: BS.ByteString
nsDirectory = narString "directory"

nsEntry :: BS.ByteString
nsEntry = narString "entry"

nsName :: BS.ByteString
nsName = narString "name"

nsNode :: BS.ByteString
nsNode = narString "node"

nsOpenParen :: BS.ByteString
nsOpenParen = narString "("

nsType :: BS.ByteString
nsType = narString "type"

narString :: BS.ByteString -> BS.ByteString
narString bs =
    mconcat $
      if padLen == 0
        then [ encodeLen bsLen, bs ]
        else [ encodeLen bsLen, bs, BS.replicate (8 - padLen) '\0' ]
  where
    bsLen :: Int
    bsLen = BS.length bs

    padLen :: Int
    padLen = bsLen `mod` 8

    encodeLen :: Int -> BS.ByteString
    encodeLen len =
       LBS.toStrict . Binary.runPut $ Binary.putWord64le (fromIntegral len)
