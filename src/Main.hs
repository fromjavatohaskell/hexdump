{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.Int                       ( Int64 )
import           Data.Bits                      ( shiftR )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import           Data.Word                      ( Word8 )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad                  ( void )
import qualified System.Environment            as E
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Builder       as B
import           System.IO                      ( BufferMode(..) )
import qualified System.IO                     as IO

data Chunk = Chunk {-# UNPACK #-} !Int64 !ByteString

chunkSize :: Int64
chunkSize = 16

chunked :: Int64 -> ByteString -> [Chunk]
chunked offset bs = if BSL.null bs
  then [Chunk offset bs]
  else case BSL.splitAt chunkSize bs of
    (as, zs) -> Chunk offset as : chunked (offset + BSL.length as) zs

filterPrintable :: Word8 -> Word8
filterPrintable x | x >= 0x20 && x <= 0x7e = x
                  | otherwise              = 0x2e

buildOffset :: Int64 -> Builder
buildOffset offset
  | offset < 0xFFFFFF
  = (B.int8HexFixed $ fromIntegral $ offset `shiftR` 16)
    <> (B.int16HexFixed $ fromIntegral offset)
  | otherwise
  = B.int64HexFixed offset

hex :: ByteString -> Builder
hex chunk = BSL.foldr singleSymbol mempty chunk
  where singleSymbol x rest = B.word8HexFixed x <> space <> rest

pad :: Int64 -> Builder
pad chunkLength
  | chunkLength >= chunkSize = mempty
  | otherwise = B.byteString
  $ BS.replicate (fromIntegral $ 3 * (chunkSize - chunkLength)) 0x20

buildChunk :: ByteString -> Builder
buildChunk chunk
  | not $ BSL.null chunk
  = hex chunk
    <> pad (BSL.length chunk)
    <> space
    <> B.char8 '>'
    <> B.lazyByteString (BSL.map filterPrintable chunk)
    <> B.char8 '<'
  | otherwise
  = mempty

space :: Builder
space = B.char8 ' '

newLine :: Builder
newLine = B.char8 '\n'

toBuilder :: Chunk -> Builder
toBuilder (Chunk offset chunk) =
  buildOffset offset <> space <> buildChunk chunk <> newLine

main :: IO ()
main = do
  args <- E.getArgs
  d    <- case listToMaybe args of
    Just filename -> BSL.readFile filename
    Nothing       -> BSL.getContents
  IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 32
  traverse_ (B.hPutBuilder IO.stdout . toBuilder) (chunked 0 d)
