{-# LANGUAGE OverloadedStrings, MonadComprehensions #-}

{-|
Module      : Codec.ImageType
Description : A library for inferring image type by looking at a file's initial bytes
License     : BSD3
Maintainer  : Baldur Blöndal <baldurpet@gmail.com>

Infers an image's type by looking at its initial values and comparing against some magic bytes:

>>> :set -XOverloadedStrings
>>> import Codec.ImageType
>>> import System.IO
>>> import qualified Data.ByteString as B
>>> 
>>> h <- openFile "/tmp/1_webp_ll.webp" ReadMode
>>> bytes <- hGet h 32
>>> B.isInfixOf "RIFF" bytes
True
>>> bytes
"RIFF\144h\SOH\NULWEBPVP8L\131h\SOH\NUL/\143\SOHK\DLE\141\&8l\219F\146\224"
>>> getFileType "/tmp/1_webp_ll.webp"
Just "webp"

Some other examples:

>>> import System.Process
>>> import System.Directory
>>> import Control.Monad
>>> let findTiffs = lines <$> readProcess "locate" ["*.tiff"] ""
>>> length <$> findTiffs
25
>>> findTiffs >>= filterM doesFileExist >>= mapM getFileType
[Just "tiff",Just "tiff", …
>>> sequence_ <$> (findTiffs >>= filterM doesFileExist >>= mapM getFileType)
Just ()
-}

module Codec.ImageType (
  -- * Actions
  getFileType,
  getFileTypes,

  -- * Predicates
  isJpeg,
  isPng,
  isGif,
  isTiff,
  isRgb,
  isPbm,
  isPgm,
  isPpm,
  isRast,
  isXbm,
  isBmp,
  isWebp,
  isExr,

  -- * Getting file type name
  testJpeg,
  testPng,
  testGif,
  testTiff,
  testRgb,
  testPbm,
  testPgm,
  testPpm,
  testRast,
  testXbm,
  testBmp,
  testWebp,
  testExr
  ) where

import Prelude hiding                   (length, head, take, drop)
import System.IO                        (withFile, IOMode(ReadMode))
import Data.ByteString                  (ByteString, length, hGet, head, take, drop, index, isPrefixOf)
import qualified Data.ByteString as BS
import Control.Monad                    (guard)
import Data.Maybe                       (isJust, catMaybes, listToMaybe)
import Control.Applicative              ((<$>))

-- | Performs an action on the first 32-bytes of a given file.
reading :: FilePath -> (ByteString -> r) -> IO r
reading file test = withFile file ReadMode $ \h -> do
  bytes <- hGet h 32
  return (length bytes `seq` test bytes)

-- | Joint Photographic Experts Group (JPEG). Returns @Just "jpeg"@ if
-- file satisfies check.
testJpeg :: ByteString -> Maybe String
testJpeg bytes = [ "jpeg"
                 | take 4 (drop 6 bytes) `elem` ["JFIF", "Exif"]
                 ]

-- | Checks if file is @jpeg@.
-- 
-- >>> import Codec.ImageType 
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>> 
-- >>> 
-- >>> getDirectoryContents "." >>= filterM doesFileExist >>= filterM isJpeg
-- ["file2.jpeg","file1.jpeg"]
-- 
isJpeg :: FilePath -> IO Bool
isJpeg file = isJust <$> reading file testJpeg

-- | Portable Network Graphics (PNG). Returns @Just "png"@ if file
-- satisfies check against magic number @89 50 4e 47 0d 0a 1a 0a@.
testPng :: ByteString -> Maybe String
testPng bytes = [ "png"
                | isPrefixOf "\137PNG\r\n\26\n" bytes
                ]

-- | Checks if file is @png@.
isPng :: FilePath -> IO Bool
isPng file = isJust <$> reading file testPng

-- | Graphics Interchange Format (GIF). Returns @Just "gif"@ if file
-- satisfies check against magic number @GIF87a@ and @GIF89a@.
testGif :: ByteString -> Maybe String
testGif bytes = [ "gif"
                | elem (take 6 bytes) ["GIF87a", "GIF89a"]
                ]

-- | Checks if file is @gif@.
isGif :: FilePath -> IO Bool
isGif file = isJust <$> reading file testGif

-- | Tagged Image File Format (TIFF). Returns @Just "tiff"@ if first
-- short is @II@ or @MM@.
testTiff :: ByteString -> Maybe String
testTiff bytes = [ "tiff"
                 | elem (take 2 bytes) ["MM", "II"]
                 ]

-- | Checks if file is @tiff@.
isTiff :: FilePath -> IO Bool
isTiff file = isJust <$> reading file testTiff

-- | SGI image library. Checks magic number (decimal value 474 as a
-- short) that identifies file as an SGI image file and then returns
-- @Just "rgb"@.
testRgb :: ByteString -> Maybe String
testRgb bytes = [ "rgb"
                | isPrefixOf "\001\218" bytes
                ]

-- | Checks if file is @rgb@.
isRgb :: FilePath -> IO Bool
isRgb file = isJust <$> reading file testRgb

-- | PBM (portable bitmap). Returns @Just "pbm"@ if file satisfies check.
testPbm :: ByteString -> Maybe String
testPbm bytes = [ "pbm"
                | length bytes >= 3
                , index bytes(0) == head "P"
                , index bytes(1) `BS.elem` "14"
                , index bytes(2) `BS.elem` " \t\n\r"
                ]

-- | Checks if file is @pbm@. 
isPbm :: FilePath -> IO Bool
isPbm file = isJust <$> reading file testPbm

-- | PGM (portable graymap). Returns @Just "pgm"@ if file satisfies check.
testPgm :: ByteString -> Maybe String
testPgm bytes = [ "pgm"
                | length bytes >= 3
                , index bytes(0) == head "P"
                , index bytes(1) `BS.elem` "25"
                , index bytes(2) `BS.elem` " \t\n\r"
                ]

-- | Checks if file is @pgm@.
isPgm :: FilePath -> IO Bool
isPgm file = isJust <$> reading file testPgm

-- | PPM (portable pixmap). Returns @Just "ppm"@ if file satisfies check.
testPpm :: ByteString -> Maybe String
testPpm bytes = [ "ppm"
                | length bytes >= 3
                , index bytes(0) == head "P"
                , index bytes(1) `BS.elem` "36"
                , index bytes(2) `BS.elem` " \t\n\r"
                ]

-- | Checks if file is @ppm@.
isPpm :: FilePath -> IO Bool
isPpm file = isJust <$> reading file testPpm

-- | Sun raster file. Returns @Just "rast"@ if file satisfies check.
testRast :: ByteString -> Maybe String
testRast bytes = [ "rast"
                 | isPrefixOf "\x59\xA6\x6A\x95" bytes
                 ]

-- | Checks if file is @rast@.
isRast :: FilePath -> IO Bool
isRast file = isJust <$> reading file testRast

-- | X bitmap (X10 or X11). Returns @Just "xbm"@ if file satisfies check.
testXbm :: ByteString -> Maybe String
testXbm bytes = [ "xbm"
                | isPrefixOf "#define " bytes
                ]

-- | Checks if file is @xbm@.
isXbm :: FilePath -> IO Bool
isXbm file = isJust <$> reading file testXbm

-- | Bitmap (BMP) file format. Returns @Just "bmp"@ if file satisfies check.
testBmp :: ByteString -> Maybe String
testBmp bytes = [ "bmp"
                | isPrefixOf "BM" bytes
                ]

-- | Checks if file is @bmp@.
isBmp :: FilePath -> IO Bool
isBmp file = isJust <$> reading file testBmp

-- | WebP. Returns @Just "webp"@ if file satisfies check.
testWebp :: ByteString -> Maybe String
testWebp bytes = [ "webp"
                 | isPrefixOf "RIFF" bytes
                 , take 4 (drop 8 bytes) == "WEBP"
                 ]

-- | Checks if file is @webp@.
isWebp :: FilePath -> IO Bool
isWebp file = isJust <$> reading file testWebp

-- | OpenEXR. Returns @Just "exr"@ if file satisfies check.
testExr :: ByteString -> Maybe String
testExr bytes = [ "exr"
                | isPrefixOf "\x76\x2f\x31\x01" bytes
                ]

-- | Checks if file is @exr@.
isExr :: FilePath -> IO Bool
isExr file = isJust <$> reading file testExr

testSvg :: ByteString -> Maybe String
testSvg bytes = [ "svg+xml"
                | isPrefixOf "<svg" bytes
                ]


tests :: [ByteString -> Maybe String]
tests = [testJpeg, testPng, testGif,
         testTiff, testRgb, testPbm,
         testPgm, testPpm, testRast,
         testXbm, testBmp, testWebp,
         testExr, testSvg]

-- | Gets a ginel possible file types based on fairly arbitrary tie
-- breaking.
-- 
-- >>> import System.Directory
-- >>> import Control.Monad
-- >>> getDirectoryContents "." >>= filterM doesFileExist >>= mapM getFileType
-- [Just "rast",Just "jpeg",Nothing,Just "webp",Just "gif",Just "pgm",Just "webp",Nothing,Just "webp",Just "exr"]
-- 
getFileType :: FilePath -> IO (Maybe String)
getFileType file = reading file $ \bytes -> do
  listToMaybe $ catMaybes [ test bytes | test <- tests ]

-- | Gets possible file types. Returns empty list if nothing is found,
-- otherwise a list of matches.
-- 
-- >>> import System.Directory
-- >>> import Control.Monad
-- >>> getDirectoryContents "." >>= filterM doesFileExist >>= mapM getFileTypes
-- [["rast"],["jpeg"],[],["webp"],["gif"],["pgm"],["webp"],[],["webp"],["exr"]]
-- 
getFileTypes :: FilePath -> IO [String]
getFileTypes file = reading file $ \bytes -> do
  catMaybes [ test bytes | test <- tests ]
