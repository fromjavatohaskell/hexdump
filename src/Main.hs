{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}


-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           GHC.Int                        ( Int64(..) )
import           GHC.Word                       ( Word64(..), Word8(..) )
import           Data.Bits                      ( Bits(..), (.&.) )
import           GHC.Prim                       ( Int#, uncheckedShiftRL#, clz64# )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Maybe                     ( listToMaybe )
import qualified System.Environment            as E
import qualified Data.ByteString.Lazy          as BSL
import           System.IO                      ( BufferMode(..) )
import qualified System.IO                     as IO
import qualified GHC.IO.Buffer                 as Buf
import           Foreign.Ptr                   ( Ptr ) 
import qualified Foreign.Storable              as Buf


chunkSize :: Int
chunkSize = 16

chunkSize64 :: Int64
chunkSize64 = fromIntegral chunkSize

(>>>) :: Word64 -> Int# -> Word64
(W64# n) >>> i = W64# (uncheckedShiftRL# n i)

(>>>|) :: Word8 -> Int# -> Word8
(W8# n) >>>| i = W8# (uncheckedShiftRL# n i)

clz :: Word64 -> Word64
clz (W64# n) = W64# (clz64# n)

getData :: Maybe String -> IO ByteString
getData (Just filename) = BSL.readFile filename
getData Nothing = BSL.getContents

setupOutputBuffering :: IO ()
setupOutputBuffering = IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 64

getFilename :: IO (Maybe String)
getFilename = fmap listToMaybe E.getArgs

hexDigit :: Word8 -> Word8
hexDigit !x = {-# SCC "hexDigit" #-}
  encodeNibble (x .&. 0xF)
{-# INLINE hexDigit #-}

encodeNibble :: Word8 -> Word8
encodeNibble !nibble 
--if nibble < 0xa then (nibble + 0x30) else (nibble + 0x57)
  | nibble < 0xa =  {-# SCC "encodeNibble" #-}  nibble + 0x30
  | otherwise =  {-# SCC "encodeNibble" #-}  nibble + 0x57
{-# INLINE encodeNibble #-}

encodeOffset :: Word64 -> Ptr Word8 -> Int -> IO Int
encodeOffset offset buffer len = {-# SCC "encodeOffset" #-} do
  let decodeNibbles = max 6 (16 - (fromIntegral ((clz offset) >>> 2#))) :: Int
  encodeOffset' offset buffer (len + decodeNibbles - 1) decodeNibbles
  return $! len + decodeNibbles

encodeOffset' :: Word64 -> Ptr Word8 -> Int -> Int -> IO ()
encodeOffset' !offset !buffer !index !decodeNibbles
  | decodeNibbles > 0 = do
    Buf.pokeByteOff buffer index (hexDigit $ fromIntegral offset)
    encodeOffset' (offset >>> 4#) buffer (index - 1) (decodeNibbles - 1)
  | otherwise = do
    return ()


writeBuffer :: Ptr Word8 -> Int -> IO ()
writeBuffer buffer len = {-# SCC "writeBuffer" #-} do
  Buf.pokeByteOff buffer len (0x0a :: Word8)
  IO.hPutBuf IO.stdout buffer (len + 1)


encode :: Word64 -> ByteString -> Ptr Word8 -> IO ()
encode offset inData buffer
  | BSL.null inData = {-# SCC "encode" #-} do
     len <- encodeOffset offset buffer 0
     writeBuffer buffer len
  | otherwise = {-# SCC "encode" #-} do
     let (as, zs) = BSL.splitAt chunkSize64 inData
     let chunkLength = (fromIntegral $ BSL.length as) :: Int
     len <- encodeOffset offset buffer 0
     Buf.pokeByteOff buffer len (0x20 :: Word8)
     let len2 = len + 1
     len3 <- encodeByte as 0 chunkLength buffer len2

     len4 <- if chunkLength < chunkSize then pad 0 (3 * (chunkSize - chunkLength)) buffer len3
        else return len3
     
     Buf.pokeByteOff buffer len4 (0x20 :: Word8)
     let len5 = len4 + 1
     Buf.pokeByteOff buffer len5 (0x3e :: Word8)
     let len6 = len5 + 1

     len7 <- encodeAscii as 0 chunkLength buffer len6
     Buf.pokeByteOff buffer len7 (0x3c :: Word8)
     let len8 = len7 + 1
     writeBuffer buffer len8
     encode (offset + fromIntegral chunkLength) zs buffer


pad :: Int -> Int -> Ptr Word8 -> Int -> IO Int
pad !bytesToPad !totalBytesToPad buffer !len
  | bytesToPad < totalBytesToPad = {-# SCC "pad" #-} do 
    Buf.pokeByteOff buffer len (0x20 :: Word8)
    pad (bytesToPad + 1) totalBytesToPad buffer (len + 1)
  | otherwise = {-# SCC "pad" #-} return len

encodeByte :: ByteString -> Int -> Int -> Ptr Word8 -> Int -> IO Int
encodeByte as !index !asLength buffer !len
  | index < asLength = {-# SCC "encodeByte" #-} do
     let oneByte = BSL.index as (fromIntegral index)
     Buf.pokeByteOff buffer len (hexDigit (oneByte >>>| 4#))
     Buf.pokeByteOff buffer (len+1) (hexDigit oneByte)
     Buf.pokeByteOff buffer (len+2) (0x20 :: Word8)
     encodeByte as (index + 1) asLength buffer (len + 3)
  | otherwise = {-# SCC "encodeByte" #-} return len

filterPrintable :: Word8 -> Word8
filterPrintable x | x >= 0x20 && x <= 0x7e = x
                  | otherwise              = 0x2e

encodeAscii :: ByteString -> Int -> Int -> Ptr Word8 -> Int -> IO Int
encodeAscii as !index !asLength buffer !len
  | index < asLength = {-# SCC "encodeAscii" #-} do
    let oneByte = BSL.index as (fromIntegral index)
    Buf.pokeByteOff buffer len $ filterPrintable oneByte
    encodeAscii as (index + 1) asLength buffer (len + 1)
  | otherwise = {-# SCC "encodeAscii" #-} return len

main :: IO ()
main = do
  setupOutputBuffering
  inData <- getFilename >>= getData
  buf <- Buf.newByteBuffer 1024 Buf.WriteBuffer
  Buf.withBuffer buf $ encode 0 inData

