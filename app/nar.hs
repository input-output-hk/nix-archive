{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe (fromMaybe)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as Text

import           Nix.Nar.Git (GitHash (..))
import qualified Nix.Nar as Nar

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt

import           System.FilePath (dropTrailingPathSeparator)

main :: IO ()
main =
  Opt.customExecParser p opts >>= runNarCommand
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pCommand)
      ( Opt.fullDesc
      <> Opt.header "nar - A tool interacting with Nix ARchive files."
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = CmdBase16ToNixSha ByteString
  | CmdGitNar GitDirectory (Maybe GitHash)
  | CmdGitNarSha GitDirectory (Maybe GitHash)
  | CmdNixSha FilePath

newtype GitDirectory
  = GitDirectory FilePath

-- -----------------------------------------------------------------------------

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption "nar version 0.1.0.0"
    (  Opt.long "version"
    <> Opt.short 'v'
    <> Opt.help "Print the version and exit"
    )

pCommand :: Parser Command
pCommand =
  Opt.subparser $ mconcat
    [ Opt.command "base16-to-nix-sha"
       ( Opt.info (CmdBase16ToNixSha <$> pBase16Sha)
       $ Opt.progDesc "Convert a base 15 SHA hash to a Nix SHA hash"
       )
    , Opt.command "git-nar"
       ( Opt.info (CmdGitNar <$> pGitDirectory <*> Opt.optional pGitHash)
       $ Opt.progDesc "Create a NAR from a git repo, at the specified git hash"
       )
    , Opt.command "git-nar-sha"
       ( Opt.info (CmdGitNarSha <$> pGitDirectory <*> Opt.optional pGitHash)
       $ Opt.progDesc "Create a NAR from a git repo, at the specified git hash"
       )
    , Opt.command "nix-sha"
       ( Opt.info (CmdNixSha <$> pFilePath)
       $ Opt.progDesc "Print the Nix SHA256 hash of the specified file."
       )
    ]

pBase16Sha :: Parser ByteString
pBase16Sha =
  BS.pack <$>
    Opt.strOption
      (  Opt.long "hash"
      <> Opt.help "The base 16 SHA hash."
      )

pFilePath :: Parser FilePath
pFilePath =
  Opt.strOption
    (  Opt.long "file"
    <> Opt.help "The file path."
    )

pGitDirectory :: Parser GitDirectory
pGitDirectory =
  GitDirectory . dropTrailingPathSeparator <$>
    Opt.strOption
      (  Opt.long "git-dir"
      <> Opt.help "The directory containing a git checkout."
      )

pGitHash :: Parser GitHash
pGitHash =
  GitHash <$>
    Opt.strOption
      (  Opt.long "hash"
      <> Opt.help "The git hash."
      )

-- -----------------------------------------------------------------------------

runNarCommand :: Command -> IO ()
runNarCommand cmd =
  case cmd of
    CmdBase16ToNixSha hash ->
      BS.putStrLn $ Nar.base16ToNix hash
    CmdGitNar (GitDirectory gitdir) mgh ->
      Text.putStrLn . Nar.renderNarStatus =<< Nar.archive gitdir (fromMaybe (GitHash "HEAD") mgh)

    CmdGitNarSha (GitDirectory gitdir) mgh ->
      BS.putStrLn . either (BS.pack . show) id =<< Nar.nixShaGitRepoAtHash gitdir (fromMaybe (GitHash "HEAD") mgh)

    CmdNixSha fpath -> BS.putStrLn =<< Nar.nixShaFile fpath
