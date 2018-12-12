{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.Int ( Int64 )
import           Data.ByteString.Lazy ( ByteString )
import           Data.ByteString.Builder ( Builder )
import           Data.Word ( Word8 )
import           Data.Maybe ( listToMaybe )
import           Control.Monad ( void )
import qualified System.Environment as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL hiding (scanl')
import qualified Data.ByteString.Builder as B
import           System.IO ( BufferMode(..) )
import qualified System.IO as IO

data Chunk = Chunk !Bool {-# UNPACK #-} !Int64 !ByteString !ByteString

chunkSize :: Int64
chunkSize = 16

initialChunk :: ByteString -> Chunk
initialChunk x = f $ BSL.splitAt chunkSize x
  where f (a, b) = Chunk True 0 a b

nextChunk :: Chunk -> Chunk
nextChunk (Chunk ne offset head rest) = f $ BSL.splitAt chunkSize rest
  where f (a, b) = Chunk (ne && prevNonEmpty) (offset + BSL.length head) a b 
        prevNonEmpty = not $ BSL.null head

notEmpty :: Chunk -> Bool
notEmpty (Chunk ne _ head _) = ne

split :: ByteString -> [Chunk]
split x = takeWhile notEmpty $ iterate nextChunk $ initialChunk x


filterPrintable :: Word8 -> Word8
filterPrintable x
  | x >= 0x20 && x <=0x7e = x
  | otherwise = 0x2e

buildOffset :: Int64 -> Builder
buildOffset offset 
  | offset < 0x7FFFFF = B.int64HexFixed offset
  | otherwise = B.int64HexFixed offset

hex :: ByteString -> Builder
hex chunk = BSL.foldr singleSymbol mempty chunk
  where singleSymbol x rest = B.word8HexFixed x <> space <> rest

pad :: Int64 -> Builder
pad chunkLength
  | chunkLength >= chunkSize = mempty
  | otherwise = B.byteString $ BS.replicate (fromInteger $ toInteger $ 3 * (chunkSize - chunkLength)) 0x20

buildChunk :: ByteString -> Builder
buildChunk chunk
  | not $ BSL.null chunk = 
    hex chunk <>
    pad (BSL.length chunk) <>
    space <>
    B.char8 '>' <>
    B.lazyByteString (BSL.map filterPrintable chunk) <>
    B.char8 '<'
  | otherwise = mempty

space :: Builder
space = B.char8 ' '

newLine :: Builder
newLine = B.char8 '\n'

toBuilder :: Chunk -> Builder
toBuilder (Chunk _ offset chunk _) = buildOffset offset <> space <> buildChunk chunk <> newLine

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BSL.readFile filename
    Nothing -> BSL.getContents
  IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 16
  void $ traverse (B.hPutBuilder IO.stdout . toBuilder) (split d)
