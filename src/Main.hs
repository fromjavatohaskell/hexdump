{-# LANGUAGE OverloadedStrings, MagicHash #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           GHC.Int                        ( Int64(..), Int8 )
import           Data.Bits                      ( shiftR , (.&.))
import           GHC.Prim                       ( Int#, uncheckedIShiftRL# )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import           Data.Word                      ( Word8 )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( traverse_ )
import           Control.Monad                  ( void )
import           Foreign                        ( ForeignPtr(..) )
import           Foreign.Storable               ( peekElemOff )
import           Foreign.ForeignPtr.Unsafe      ( unsafeForeignPtrToPtr )
import           System.IO.Unsafe               ( unsafePerformIO )
import qualified System.Environment            as E
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Internal      as BS
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

uncheckedIShiftRL :: Int64 -> Int# -> Int64
uncheckedIShiftRL (I64# n) i = I64# (uncheckedIShiftRL# n i)

buildOffset :: Int64 -> Builder
buildOffset offset
  | offset < 0xFFFFFF
  = (B.int8HexFixed $ fromIntegral $ offset `uncheckedIShiftRL` 16#)
    <> (B.int16HexFixed $ fromIntegral offset)
  | otherwise
  = B.int64HexFixed offset

toBytes :: Int64 -> [Int8]
toBytes = undefined

newtype EncodingTable = EncodingTable (ForeignPtr Word8)

tableFromList :: [Word8] -> EncodingTable
tableFromList xs = case BS.pack xs of BS.PS fp _ _ -> EncodingTable fp

unsafeIndex :: EncodingTable -> Int -> IO Word8
unsafeIndex (EncodingTable table) = peekElemOff (unsafeForeignPtrToPtr table)

lowerAlphabet :: EncodingTable
lowerAlphabet =
    tableFromList $ map (fromIntegral . fromEnum) $ ['0'..'9'] ++ ['a'..'f']

hexEncodeLowerNibble :: Int -> Word8
hexEncodeLowerNibble x = unsafePerformIO $ unsafeIndex lowerAlphabet $ x .&. 0xF


hex :: ByteString -> Builder
hex chunk = BSL.foldr singleSymbol mempty chunk
  where singleSymbol x rest = B.word8HexFixed x <> space <> rest

pad :: Int64 -> Builder
pad chunkLength
  | chunkLength >= chunkSize = mempty
  | otherwise = foldr mappend mempty $ replicate padSize space
     where padSize = fromIntegral $ 3 * (chunkSize - chunkLength)

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
  B.hPutBuilder IO.stdout $ buildOffset 0x0 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x1 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x12 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x123 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x1234 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x12345 <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0xFFFFFF <> newLine
  B.hPutBuilder IO.stdout $ B.word8 (hexEncodeLowerNibble 0x12345) <> newLine
  B.hPutBuilder IO.stdout $ buildOffset 0x1000000 <> newLine
