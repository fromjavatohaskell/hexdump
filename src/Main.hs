{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           GHC.Word                       ( Word64(..), Word8(..) )
import           Data.Bits                      ( Bits(..), (.&.) )
import           GHC.Prim                       ( Int#, uncheckedShiftRL#, clz64# )
import           Data.Maybe                     ( listToMaybe )
import           Control.Monad                  ( when )
import qualified System.Environment            as E
import           System.IO                      ( BufferMode(..) )
import qualified System.IO                     as IO
import qualified GHC.IO.Buffer                 as Buf
import           Foreign.Ptr                   ( Ptr ) 
import           Foreign                   ( ForeignPtr ) 
import qualified Foreign.Storable              as Buf
import           Foreign.ForeignPtr.Unsafe     ( unsafeForeignPtrToPtr )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS

{-# NOINLINE table #-}
table :: ForeignPtr Word8
table =
   case BS.pack $ [48..(48+9)] ++ [97..(97+5)]
     of BS.PS fp _ _ -> fp

{-# INLINE tableHex #-}
tableHex :: Int -> IO Word8
tableHex !index = Buf.peekByteOff (unsafeForeignPtrToPtr table) index

chunkSize :: Int
chunkSize = 16

(>>>) :: Word64 -> Int# -> Word64
(W64# n) >>> i = W64# (uncheckedShiftRL# n i)

(>>>|) :: Word8 -> Int# -> Word8
(W8# n) >>>| i = W8# (uncheckedShiftRL# n i)

clz :: Word64 -> Word64
clz (W64# n) = W64# (clz64# n)

getHandle :: Maybe String -> IO IO.Handle
getHandle (Just filename) = IO.openFile filename IO.ReadMode
getHandle Nothing = return IO.stdin

getFilename :: IO (Maybe String)
getFilename = fmap listToMaybe E.getArgs

hexDigit :: Word8 -> IO Word8
hexDigit !x = do
  let nibble = x .&. 0xF
  tableHex $ fromIntegral nibble
{-# INLINE hexDigit #-}

encodeOffset :: Word64 -> Ptr Word8 -> Int -> IO Int
encodeOffset offset buffer len = do
  let decodeNibbles = max 6 (16 - (fromIntegral ((clz offset) >>> 2#))) :: Int
  encodeOffset' offset (len + decodeNibbles - 1) decodeNibbles
  return $! len + decodeNibbles
 where 
  encodeOffset' !offset' !index !decodeNibbles
   | decodeNibbles > 0 = do
     !hexTemp <- hexDigit $! fromIntegral offset'
     Buf.pokeByteOff buffer index hexTemp
     encodeOffset' (offset' >>> 4#) (index - 1) (decodeNibbles - 1)
   | otherwise = do
     return ()
{-# INLINE encodeOffset #-}


encode :: Word64 -> Ptr Word8 -> Int -> Ptr Word8 -> IO (Int)
encode !offset bufferIn !chunkLength bufferOut
  | (chunkLength == 0) = do
     len <- encodeOffset offset bufferOut 0
     Buf.pokeByteOff bufferOut len (0x0a :: Word8)
     return (len + 1)
  | otherwise = do
     len <- encodeOffset offset bufferOut 0
     Buf.pokeByteOff bufferOut len (0x20 :: Word8)
     let len2 = len + 1
     len3 <- encodeByte 0 len2

     len4 <- if chunkLength < chunkSize then pad 0 (3 * (chunkSize - chunkLength)) len3
        else return len3
     
     Buf.pokeByteOff bufferOut len4 (0x20 :: Word8)
     let len5 = len4 + 1
     Buf.pokeByteOff bufferOut len5 (0x3e :: Word8)
     let len6 = len5 + 1

     len7 <- encodeAscii 0 len6
     Buf.pokeByteOff bufferOut len7 (0x3c :: Word8)
     let len8 = len7 + 1
     Buf.pokeByteOff bufferOut len8 (0x0a :: Word8)
     let len9 = len8 + 1
     return len9
  where
 encodeByte !index !len
  | index < chunkLength = do
     oneByte <- (Buf.peekByteOff bufferIn index) :: IO Word8
     hexTemp1 <- hexDigit (oneByte >>>| 4#)
     hexTemp2 <- hexDigit oneByte
     Buf.pokeByteOff bufferOut len hexTemp1
     Buf.pokeByteOff bufferOut (len+1) hexTemp2
     Buf.pokeByteOff bufferOut (len+2) (0x20 :: Word8)
     encodeByte (index + 1) (len + 3)
  | otherwise = return len
 encodeAscii !index !len
  | index < chunkLength = do
    oneByte <- (Buf.peekByteOff bufferIn index) :: IO Word8
    filteredAscii <- tableConvertAscii $ fromIntegral oneByte
    Buf.pokeByteOff bufferOut len filteredAscii
    encodeAscii (index + 1) (len + 1)
  | otherwise = return len
 pad !bytesToPad totalBytesToPad !len
  | bytesToPad < totalBytesToPad = do
    Buf.pokeByteOff bufferOut len (0x20 :: Word8)
    pad (bytesToPad + 1) totalBytesToPad (len + 1)
  | otherwise = return len
{-# NOINLINE encode #-}

encodeStream :: Word64 -> IO.Handle -> Ptr Word8 -> Ptr Word8 -> IO ()
encodeStream offset h bufferIn bufferOut = do
  chunkLength <- IO.hGetBuf h bufferIn chunkSize
  len <- encode offset bufferIn chunkLength bufferOut
  IO.hPutBuf IO.stdout bufferOut len
  when (chunkLength > 0) $ encodeStream (offset + fromIntegral chunkLength) h bufferIn bufferOut

filterPrintable :: Int -> Word8
filterPrintable x = if (x >= 0x20 && x <= 0x7e) then (fromIntegral x) else 0x2e

{-# NOINLINE tableAscii #-}
tableAscii :: ForeignPtr Word8
tableAscii =
   case BS.pack $ fmap filterPrintable [0..255]
     of BS.PS fp _ _ -> fp

{-# INLINE tableConvertAscii #-}
tableConvertAscii :: Int -> IO Word8
tableConvertAscii !index = Buf.peekByteOff (unsafeForeignPtrToPtr tableAscii) index


main :: IO ()
main = do
  h <- getFilename >>= getHandle
  setBuffering IO.stdout
  setBuffering h
  bufIn <- Buf.newByteBuffer chunkSize Buf.WriteBuffer
  bufOut <- Buf.newByteBuffer 1024 Buf.WriteBuffer
  Buf.withBuffer bufIn  $ \ bufferIn -> Buf.withBuffer bufOut $ \ bufferOut -> do
    encodeStream 0 h bufferIn bufferOut
 where
    setBuffering handle = IO.hSetBuffering handle $ BlockBuffering $ Just $ 1024 * 64
