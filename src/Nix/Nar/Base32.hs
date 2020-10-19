{-# LANGUAGE OverloadedStrings #-}
module Nix.Nar.Base32
  ( Sha256 (..)
  , base16ToNix
  , encode
  ) where

import           Data.Bits ((.&.), (.|.), unsafeShiftL, unsafeShiftR)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word8)

{-

https://github.com/NixOS/nix/blob/583d06385de82ab5c7fc77d26cd138d3c6d5f4b5/src/libutil/hash.cc#L76

static string printHash32(const Hash & hash)
{
    assert(hash.hashSize);
    size_t len = hash.base32Len();
    assert(len);

    string s;
    s.reserve(len);

    for (int n = (int) len - 1; n >= 0; n--) {
        unsigned int b = n * 5;
        unsigned int i = b / 8;
        unsigned int j = b % 8;
        unsigned char c =
            (hash.hash[i] >> j)
            | (i >= hash.hashSize - 1 ? 0 : hash.hash[i + 1] << (8 - j));
        s.push_back(base32Chars[c & 0x1f]);
    }

    return s;
}
-}

newtype Sha256
  = Sha256 ByteString

base16ToNix :: ByteString -> ByteString
base16ToNix bs =
  case Base16.decode bs of
    Left _ -> mconcat [ "Not able to Base16.decode: '", bs, "'." ]
    Right raw -> encode (Sha256 raw)

-- Quick and dirty transliteration of the C++ code.
-- Sha256 is a 32 byte hash, but for some reason we need to add 20 trailing '\0' bytes
-- to get a 52 byte encoding.
encode :: Sha256 -> ByteString
encode (Sha256 bs) =
    BS8.pack $ List.foldl' func [] [0 .. 51]
  where
    -- Add trailing '0's to make sure the input is the correct length.
    -- The ByteString can be longer that 52 bytes, but not shorter.
    hash :: ByteString
    hash = bs <> BS.replicate (51 - BS.length bs) 0

    func :: String -> Int -> String
    func acc n =
      let (i, j) = divMod (n * 5) 8
          c = BS.index hash i `unsafeShiftR` j
                .|. if i >= BS.length hash - 1
                      then 0
                      else BS.index hash (i + 1) `unsafeShiftL` (8 - j)
      -- The Map.lookup should *never* return Nothing but we have to deal with
      -- that possibility.
      in maybe acc (:acc) $ Map.lookup (c .&. 0x1f) base32CharsRevMap

-- -----------------------------------------------------------------------------

base32Chars :: String
base32Chars = "0123456789abcdfghijklmnpqrsvwxyz"

base32CharsRevMap :: Map Word8 Char
base32CharsRevMap = Map.fromList $ zip [ 0 .. ] base32Chars

