{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           GHC.Int                        ( Int64(..) )
import           Data.Bits                      ( (.&.) )
import           GHC.Prim                       ( uncheckedIShiftRL# )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.ByteString.Builder        ( Builder )
import           Data.Word                      ( Word8 )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( traverse_ )
import qualified System.Environment            as E
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Builder       as B
import           System.IO                      ( BufferMode(..) )
import qualified System.IO                     as IO

data Chunk = Chunk Int64 ByteString

chunkSize :: Int64
chunkSize = 16

chunked :: Int64 -> ByteString -> [Chunk]
chunked offset bs = {-# SCC "chunked" #-} if BSL.null bs
  then [Chunk offset bs]
  else case BSL.splitAt chunkSize bs of
    (as, zs) -> Chunk offset as : (let offset' = offset + BSL.length as in 
                                   offset' `seq` zs `seq` chunked offset' zs )

filterPrintable :: Word8 -> Word8
filterPrintable x | x >= 0x20 && x <= 0x7e = x
                  | otherwise              = 0x2e

hexEncodeLowerNibble :: Int64 -> Word8
hexEncodeLowerNibble x = {-# SCC "hexEncode" #-}
  let nibble = fromIntegral $ x .&. 0xF in
  if nibble < 0xa then (charNumberOffset + nibble) else (charLetterOffset + nibble)
  where
    charNumberOffset = fromIntegral $ fromEnum '0'
    charLetterOffset = fromIntegral $ fromEnum 'a' - 0xa

buildOffset :: Int64 -> Builder
buildOffset offset = {-# SCC "buildOffset" #-} buildOffset' (offset >>> 24#)
  <> (B.int8HexFixed $ fromIntegral $ (offset >>> 16#))
  <> (B.int16HexFixed $ fromIntegral offset)
  where 
    (I64# n) >>> i = I64# (uncheckedIShiftRL# n i)
    buildOffset' !offset
      | offset /= 0 = (buildOffset' $! (offset >>> 4#)) <> (B.word8 $! (hexEncodeLowerNibble offset))
      | otherwise = mempty

hex :: ByteString -> Builder
hex chunk = {-# SCC "hexChunk" #-} BSL.foldr singleSymbol mempty chunk
  where singleSymbol x rest = B.word8HexFixed x <> space <> rest

pad :: Int64 -> Builder
pad chunkLength
  | chunkLength >= chunkSize = mempty
  | otherwise = mconcat $ replicate padSize space
     where padSize = fromIntegral $ 3 * (chunkSize - chunkLength)

buildChunk :: ByteString -> Builder
buildChunk chunk
  | not $ BSL.null chunk
  = {-# SCC "buildChunk" #-} space
    <> hex chunk
    <> pad (BSL.length chunk)
    <> space
    <> charBiggerThen
    <> B.lazyByteString (BSL.map filterPrintable chunk)
    <> charLessThen
  | otherwise
  = mempty

charBiggerThen :: Builder
charBiggerThen = B.char8 '>'

charLessThen :: Builder
charLessThen = B.char8 '<'

space :: Builder
space = B.char8 ' '

newLine :: Builder
newLine = B.char8 '\n'

toBuilder :: Chunk -> Builder
toBuilder (Chunk offset chunk) = {-# SCC "toBuilder" #-} buildOffset offset <> buildChunk chunk <> newLine

getData :: Maybe String -> IO ByteString
getData (Just filename) = {-# SCC "getData" #-} BSL.readFile filename
getData Nothing = {-# SCC "getData" #-} BSL.getContents

printHex :: ByteString -> IO ()
printHex dataIn = {-# SCC "printHex" #-} traverse_ (B.hPutBuilder IO.stdout . toBuilder) (chunked 0 dataIn)

main :: IO ()
main = setupOutputBuffering >> getFilename >>= getData >>= printHex
  where
    setupOutputBuffering = IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 64
    getFilename = fmap listToMaybe E.getArgs
