module Nix.Nar.Object
  ( NarObject (..)
  , gitToNarObjects
  , printNarObjects
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import           Nix.Git

import           System.FilePath (dropTrailingPathSeparator, splitPath)

import           Text.Show.Pretty (pPrint)


-- | NarObject is a nested tree representation of a directory structure.
-- It seems symlinks are not represented, but instead uses the dereferenced version of
-- the file.
data NarObject
  = NarFile FilePath GitHash
  | NarDir FilePath [NarObject]
  deriving (Eq, Read, Show)

-- Convert a flat list of GitObject into a nested tree of NarObject.
gitToNarObjects :: [GitObject] -> [NarObject]
gitToNarObjects =
  buildTree . groupOnHeadDir . map (dirAnnotate 0)

-- Useful for debugging.
printNarObjects :: [NarObject] -> IO ()
printNarObjects = pPrint

-- -----------------------------------------------------------------------------

buildTree :: [([FilePath], [GitObject])] -> [NarObject]
buildTree =
    concatMap (convert 0)
  where
    convert :: Int -> ([FilePath], [GitObject]) -> [NarObject]
    convert dropCount (fps, gos) =
      case fps of
        [] -> map narFile gos
        (d:_) -> [NarDir (dropTrailingPathSeparator d) (recurse (dropCount + 1) gos)]

    recurse :: Int -> [GitObject] -> [NarObject]
    recurse dropCount gos =
      concatMap (convert dropCount) . groupOnHeadDir $ map (dirAnnotate dropCount) gos

dirAnnotate :: Int -> GitObject -> ([FilePath], GitObject)
dirAnnotate dropCount obj = (init (drop dropCount . splitPath $ goName obj), obj)

groupOnHeadDir :: [([FilePath], GitObject)] -> [([FilePath], [GitObject])]
groupOnHeadDir =
    map collapseGroup
      . NonEmpty.groupBy (\ a b -> headMaybe (fst a) == headMaybe (fst b))
  where
    collapseGroup :: NonEmpty ([FilePath], GitObject) -> ([FilePath], [GitObject])
    collapseGroup xs =
      (fst $ NonEmpty.head xs, map snd $ NonEmpty.toList xs)

headMaybe :: [a] -> Maybe a
headMaybe xs =
  case xs of
    [] -> Nothing
    (x:_) -> Just x

narFile :: GitObject -> NarObject
narFile obj = NarFile (goName obj) (goHash obj)

