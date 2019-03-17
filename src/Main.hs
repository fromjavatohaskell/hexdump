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
import           Foreign                   ( ForeignPtr ) 
import qualified Foreign.Storable              as Buf
import           Foreign.ForeignPtr.Unsafe     ( unsafeForeignPtrToPtr )
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS

{-
-- | Simple for loop.  Counts from /start/ to /end/-1.
for :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for n0 !n f = loop n0
  where
    loop i | i == n    = return ()
           | otherwise = f i >> loop (i+1)
{-# INLINE for #-}
-}


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

hexDigit :: Word8 -> IO Word8
hexDigit !x = do
  let nibble = x .&. 0xF
  tableHex $ fromIntegral nibble
{-# INLINE hexDigit #-}

encodeOffset :: Word64 -> Ptr Word8 -> Int -> IO Int
encodeOffset offset buffer len = do
  let decodeNibbles = max 6 (16 - (fromIntegral ((clz offset) >>> 2#))) :: Int
  encodeOffset' offset buffer (len + decodeNibbles - 1) decodeNibbles
  return $! len + decodeNibbles
{-# INLINE encodeOffset #-}

encodeOffset' :: Word64 -> Ptr Word8 -> Int -> Int -> IO ()
encodeOffset' !offset !buffer !index !decodeNibbles
  | decodeNibbles > 0 = do
    !hexTemp <- hexDigit $! fromIntegral offset
    Buf.pokeByteOff buffer index hexTemp
    encodeOffset' (offset >>> 4#) buffer (index - 1) (decodeNibbles - 1)
  | otherwise = do
    return ()
{-# INLINE encodeOffset' #-}


encode :: Word64 -> ByteString -> Int -> Ptr Word8 -> IO (Int)
encode offset inData chunkLength buffer
  | BSL.null inData = do
     len <- encodeOffset offset buffer 0
     Buf.pokeByteOff buffer len (0x0a :: Word8)
     return (len + 1)
  | otherwise = do
     len <- encodeOffset offset buffer 0
     Buf.pokeByteOff buffer len (0x20 :: Word8)
     let len2 = len + 1
     len3 <- encodeByte 0 len2

     len4 <- if chunkLength < chunkSize then pad 0 (3 * (chunkSize - chunkLength)) len3
        else return len3
     
     Buf.pokeByteOff buffer len4 (0x20 :: Word8)
     let len5 = len4 + 1
     Buf.pokeByteOff buffer len5 (0x3e :: Word8)
     let len6 = len5 + 1

     len7 <- encodeAscii 0 len6
     Buf.pokeByteOff buffer len7 (0x3c :: Word8)
     let len8 = len7 + 1
     Buf.pokeByteOff buffer len8 (0x0a :: Word8)
     let len9 = len8 + 1
     return len9
  where
 encodeByte !index !len
  | index < chunkLength = do
     let oneByte = BSL.index inData (fromIntegral index)
     hexTemp1 <- hexDigit (oneByte >>>| 4#)
     hexTemp2 <- hexDigit oneByte
     Buf.pokeByteOff buffer len hexTemp1
     Buf.pokeByteOff buffer (len+1) hexTemp2
     Buf.pokeByteOff buffer (len+2) (0x20 :: Word8)
     encodeByte (index + 1) (len + 3)
  | otherwise = return len
 encodeAscii !index !len
  | index < chunkLength = do
    let oneByte = BSL.index inData (fromIntegral index)
    filteredAscii <- tableConvertAscii $ fromIntegral oneByte
    Buf.pokeByteOff buffer len filteredAscii
    encodeAscii (index + 1) (len + 1)
  | otherwise = return len
 pad !bytesToPad totalBytesToPad !len
  | bytesToPad < totalBytesToPad = do
    Buf.pokeByteOff buffer len (0x20 :: Word8)
    pad (bytesToPad + 1) totalBytesToPad (len + 1)
  | otherwise = return len
{-# NOINLINE encode #-}

encodeStream :: Word64 -> ByteString -> Ptr Word8 -> IO ()
encodeStream offset inData buffer
  | BSL.null inData = do
     len <- encode offset inData 0 buffer
     IO.hPutBuf IO.stdout buffer len
  | otherwise = do
     let (as, zs) = BSL.splitAt chunkSize64 inData
     let chunkLength = (fromIntegral $ BSL.length as) :: Int
     len <- encode offset as chunkLength buffer
     IO.hPutBuf IO.stdout buffer len
     encodeStream (offset + fromIntegral chunkLength) zs buffer

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
  setupOutputBuffering
  inData <- getFilename >>= getData
  buf <- Buf.newByteBuffer 1024 Buf.WriteBuffer
  Buf.withBuffer buf $ encodeStream 0 inData

