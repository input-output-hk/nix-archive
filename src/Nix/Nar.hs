{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar where

import           Control.Exception (IOException, try)

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Control.Monad.Trans.Except.Extra (left, firstExceptT, newExceptT)

import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as Text

import           Nix.Git
import           Nix.Nar.Object

import           System.Directory (doesDirectoryExist, setCurrentDirectory)
import           System.FilePath (takeFileName)
import           System.IO (Handle, IOMode (..), withFile)

data NarStatus
  = NarSuccess FilePath
  | NarNotDir FilePath
  | NarGitError GitError
  | NarReadFail FilePath String
  deriving Show

archive :: FilePath -> GitHash -> IO NarStatus
archive dir ghash = do
  res <- runExceptT $ archiveExcept dir ghash
  case res of
    Left err -> pure err
    Right fp -> pure $ NarSuccess fp

renderNarStatus :: NarStatus -> Text
renderNarStatus ns =
  case ns of
    NarSuccess fp -> mconcat [ "Generated nar file: ", Text.pack fp ]
    NarNotDir fp -> mconcat [ "Provided fil epath ", Text.pack fp, " is not a directory" ]
    NarGitError ge -> renderGitError ge
    NarReadFail fp err -> mconcat [ "Reading file ", Text.pack fp, " failed: ", Text.pack err ]

-- -------------------------------------------------------------------------------------------------

archiveExcept :: FilePath -> GitHash -> ExceptT NarStatus IO FilePath
archiveExcept dir ghash = do
  isDir <- liftIO $ doesDirectoryExist dir
  unless isDir $
    left $ NarNotDir dir
  let fpath = takeFileName dir ++ BS.unpack (unGitHash ghash) ++ ".nar"
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

writeNarTopLevel :: Handle -> [NarObject] -> ExceptT NarStatus IO ()
writeNarTopLevel h objs = do
  mapM_ (liftIO . BS.hPutStr h) $
    [ narHeader
    , narString "("
    , narString "type"
    , narString "directory"
    ]
  mapM_ (writeNarObject h) objs
  liftIO $ BS.hPutStr h (narString ")")

writeNarObject :: Handle -> NarObject -> ExceptT NarStatus IO ()
writeNarObject h obj = do
  case obj of
    NarFile name _ghash -> writeNarFile h name
    NarDir name objs -> writeNarDir h name objs


writeNarFile :: Handle -> FilePath -> ExceptT NarStatus IO ()
writeNarFile h fpath = do
  bs <- readFileBS fpath
  mapM_ (liftIO . BS.hPutStr h) $
    [ narString "entry"
    , narString "("
    , narString "name"
    , narString (BS.pack $ takeFileName fpath)
    , narString "node"
    , narString "("
    , narString "type"
    , narString "regular"
    , narString "contents"
    , narString bs
    , narString ")"
    , narString ")"
    ]

writeNarDir :: Handle -> String -> [NarObject] -> ExceptT NarStatus IO ()
writeNarDir h name objs = do
  mapM_ (liftIO . BS.hPutStr h) $
    [ narString "entry"
    , narString "("
    , narString "name"
    , narString (BS.pack name)
    , narString "node"
    , narString "("
    , narString "type"
    , narString "directory"
    ]
  mapM_ (writeNarObject h) objs
  mapM_ (liftIO . BS.hPutStr h) $
    [ narString ")"
    , narString ")"
    ]

readFileBS :: FilePath -> ExceptT NarStatus IO BS.ByteString
readFileBS fp =
    firstExceptT convert . newExceptT $ liftIO (try $ BS.readFile fp)
  where
    convert :: IOException -> NarStatus
    convert = NarReadFail fp . show

narHeader :: BS.ByteString
narHeader =
  "\x0d\x00\x00\x00\x00\x00\x00\x00nix-archive-1\x00\x00\x00"

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
