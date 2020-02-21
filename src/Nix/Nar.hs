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
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

import           Nix.Git
import           Nix.Nar.Object

import           System.Directory (doesDirectoryExist, setCurrentDirectory, withCurrentDirectory)
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
archiveExcept dir gHashOrig = do
  isDir <- liftIO $ doesDirectoryExist dir
  unless isDir $
    left $ NarNotDir dir
  ghash <- gitCheckHash dir gHashOrig
  let fpath = takeFileName dir ++ "-" ++ BS.unpack (unGitHash ghash) ++ ".nar"
  newExceptT .
    withFile fpath WriteMode $ \ handle -> do
      setCurrentDirectory dir
      runExceptT $ do
        firstExceptT NarGitError gitAssertDir
        objs <- firstExceptT NarGitError (gitListHash ghash)
        let nobjs = gitToNarObjects objs
        unless False .
          -- Debug
          liftIO $ do
            putStrLn ""
            printNarObjects nobjs
            putStrLn ""
        writeNarTopLevel handle nobjs
  pure fpath

gitCheckHash :: FilePath -> GitHash -> ExceptT NarStatus IO GitHash
gitCheckHash dir ghash =
  if BS.all Char.isHexDigit (unGitHash ghash)
    then pure ghash
    else newExceptT .
           withCurrentDirectory dir .
             runExceptT $
               firstExceptT NarGitError gitHeadHash


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
    NarFile name ghash -> writeNarFile h name ghash
    NarDir name objs -> writeNarDir h name objs


writeNarFile :: Handle -> FilePath -> GitHash -> ExceptT NarStatus IO ()
writeNarFile h fpath ghash = do
  bs <- firstExceptT NarGitError $ gitCatFile ghash
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



wibble :: [NarObject]
wibble =
            [ NarFile
                "Binary.hs" (GitHash "ead7c065706adbe2da41391e10d504409c8ae743")
            , NarDir
                "Binary"
                [ NarFile
                    "Annotated.hs" (GitHash "0a022f00f1d2aa809272fb4b7926a4262a85e8ed")
                , NarFile
                    "Deserialize.hs"
                    (GitHash "bb6566682589a02ef281db99a696d6b0cf5c7878")
                , NarFile
                    "Drop.hs" (GitHash "44b292fac755144e828abdc65bc6cfbd8b9af75b")
                , NarFile
                    "FromCBOR.hs" (GitHash "c0c4e0d697a859a8f517fa915dd196d458b18969")
                , NarFile
                    "Raw.hs" (GitHash "69f430649a8db4064ffa735102aeacf559e3ec89")
                , NarFile
                    "Serialize.hs" (GitHash "92444be34b7b03e65bd35e54921ab797cf770891")
                , NarFile
                    "ToCBOR.hs" (GitHash "38a20582938535f8867f59048972134b6f709366")
                ]
            ]
