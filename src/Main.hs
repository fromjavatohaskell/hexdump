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
   case BS.pack [0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x61,0x62,0x63,0x64,0x65,0x66] 
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
--hexDigit !x = {-# SCC "hexDigit" #-} do
hexDigit !x = do
  let nibble = x .&. 0xF
--  return $! if nibble < 0xa then (nibble + 0x30) else (nibble + 0x57)
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


writeBuffer :: Ptr Word8 -> Int -> IO ()
writeBuffer buffer len = {-# SCC "writeBuffer" #-} do
  Buf.pokeByteOff buffer len (0x0a :: Word8)
  IO.hPutBuf IO.stdout buffer (len + 1)


encode :: Word64 -> ByteString -> Ptr Word8 -> IO ()
encode offset inData buffer
  | BSL.null inData = do
     len <- encodeOffset offset buffer 0
     writeBuffer buffer len
  | otherwise = do
     let (as, zs) = BSL.splitAt chunkSize64 inData
     let chunkLength = (fromIntegral $ BSL.length as) :: Int
     len <- encodeOffset offset buffer 0
     Buf.pokeByteOff buffer len (0x20 :: Word8)
     let len2 = len + 1
     len3 <- encodeByte as chunkLength buffer len2

     len4 <- if chunkLength < chunkSize then pad 0 (3 * (chunkSize - chunkLength)) buffer len3
        else return len3
     
     Buf.pokeByteOff buffer len4 (0x20 :: Word8)
     let len5 = len4 + 1
     Buf.pokeByteOff buffer len5 (0x3e :: Word8)
     let len6 = len5 + 1

     len7 <- encodeAscii as chunkLength buffer len6
     Buf.pokeByteOff buffer len7 (0x3c :: Word8)
     let len8 = len7 + 1
     writeBuffer buffer len8
     encode (offset + fromIntegral chunkLength) zs buffer


pad :: Int -> Int -> Ptr Word8 -> Int -> IO Int
pad !bytesToPad !totalBytesToPad buffer !len
  | bytesToPad < totalBytesToPad = do 
    Buf.pokeByteOff buffer len (0x20 :: Word8)
    pad (bytesToPad + 1) totalBytesToPad buffer (len + 1)
  | otherwise = return len


encodeByte :: ByteString -> Int -> Ptr Word8 -> Int -> IO Int
encodeByte as chunkLength buffer len = {-# SCC "encodeByte" #-} encodeByte' as 0 chunkLength buffer len

encodeByte' :: ByteString -> Int -> Int -> Ptr Word8 -> Int -> IO Int
encodeByte' as !index !asLength buffer !len
  | index < asLength = do
     let oneByte = BSL.index as (fromIntegral index)
     !hexTemp1 <- hexDigit $! (oneByte >>>| 4#)
     !hexTemp2 <- hexDigit oneByte
     Buf.pokeByteOff buffer len hexTemp1
     Buf.pokeByteOff buffer (len+1) hexTemp2
     Buf.pokeByteOff buffer (len+2) (0x20 :: Word8)
     encodeByte' as (index + 1) asLength buffer (len + 3)
  | otherwise = return len
{-# INLINE encodeByte' #-}

filterPrintable :: Word8 -> IO Word8
filterPrintable x = do
  return $! if (x >= 0x20 && x <= 0x7e) then x else 0x2e
{-# INLINE filterPrintable #-}

encodeAscii :: ByteString -> Int -> Ptr Word8 -> Int -> IO Int
encodeAscii as chunkLength buffer len = {-# SCC "encodeAscii" #-} encodeAscii' as 0 chunkLength buffer len 

encodeAscii' :: ByteString -> Int -> Int -> Ptr Word8 -> Int -> IO Int
encodeAscii' as !index !asLength buffer !len
  | index < asLength = do
    let oneByte = BSL.index as (fromIntegral index)
    !filteredAscii <- filterPrintable oneByte
    Buf.pokeByteOff buffer len filteredAscii
    encodeAscii' as (index + 1) asLength buffer (len + 1)
  | otherwise = return len
{-# INLINE encodeAscii' #-}

main :: IO ()
main = do
  setupOutputBuffering
  inData <- getFilename >>= getData
  buf <- Buf.newByteBuffer 1024 Buf.WriteBuffer
  Buf.withBuffer buf $ {-# SCC "encode" #-} encode 0 inData

