module Nix.Nar.Object
  ( NarFileType (..)
  , NarObject (..)
  , gitToNarObjects
  , printNarObjects
  ) where

import           Data.Bits ((.&.))
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import           Nix.Nar.Git

import           System.FilePath (dropTrailingPathSeparator, splitPath, takeFileName)

import           Text.Show.Pretty (pPrint)


-- | NarObject is a nested tree representation of a directory structure.
-- It seems symlinks are not represented, but instead uses the dereferenced version of
-- the file.
data NarObject
  = NarDir FilePath [NarObject]
  | NarFile FilePath GitHash NarFileType
  | NarSubModule FilePath GitHash
  deriving (Eq, Read, Show)

data NarFileType
  = NarRegular
  | NarExecuatble  -- Actually a regular file but also an executable
  | NarSymlink
  deriving (Eq, Read, Show)

-- Need a custom Ord instance to get the sort order the same as 'nix-store'.
-- All compares that are already correct can just be 'EQ'.
instance Ord NarObject where
  compare a b =
    case (a, b) of
      (NarDir afp _, NarDir bfp _) -> compare afp bfp
      (NarFile afp _ _, NarFile bfp _ _) -> compare afp bfp
      (NarSubModule afp _, NarSubModule bfp _) -> compare afp bfp

      (NarDir afp _, NarFile bfp _ _) -> compare afp bfp
      (NarDir afp _, NarSubModule bfp _) -> compare afp bfp
      (NarFile afp _ _, NarDir bfp _) -> compare afp bfp
      (NarFile afp _ _, NarSubModule bfp _) -> compare afp bfp
      (NarSubModule afp _, NarDir bfp _) -> compare afp bfp
      (NarSubModule afp _, NarFile bfp _ _) -> compare afp bfp

-- Convert a flat list of GitObject into a nested tree of NarObject.
gitToNarObjects :: [GitObject] -> [NarObject]
gitToNarObjects =
  reorder . buildTree . groupOnHeadDir . map (dirAnnotate 0)

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
narFile obj =
  case goType obj of
    GOTBlob -> NarFile (takeFileName $ goName obj) (goHash obj) (gitObjToNarFileType (goPerms obj))
    GOTCommit -> NarDir (takeFileName $ goName obj) [NarSubModule (goName obj) (goHash obj)]

  where
    gitObjToNarFileType ::  Word -> NarFileType
    gitObjToNarFileType w
      | w .&. 1 > 0 = NarExecuatble
      | w == 120000 = NarSymlink
      | otherwise = NarRegular

-- Recursively sort the tree to match the 'nix-store' version.
reorder :: [NarObject] -> [NarObject]
reorder =
    List.sort . map reorderObj
  where
    reorderObj :: NarObject -> NarObject
    reorderObj no =
      case no of
        NarDir fp objs -> NarDir fp (reorder objs)
        NarFile {} -> no
        NarSubModule {} -> no
