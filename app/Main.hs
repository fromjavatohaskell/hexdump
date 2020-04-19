{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Foreign.Ptr (Ptr)
import qualified GHC.IO.Buffer as Buf
import GHC.Word (Word64 (..), Word8 (..))
import Hexdump.Encode
import qualified System.Environment as E
import System.IO (BufferMode (..))
import qualified System.IO as IO

encodeStream :: Word64 -> IO.Handle -> Ptr Word8 -> Ptr Word8 -> IO ()
encodeStream offset h bufferIn bufferOut = do
  chunkLength <- IO.hGetBuf h bufferIn chunkSize
  len <- encode offset bufferIn chunkLength bufferOut
  IO.hPutBuf IO.stdout bufferOut len
  when (chunkLength > 0) $ encodeStream (offset + fromIntegral chunkLength) h bufferIn bufferOut

main :: IO ()
main = do
  h <- fmap listToMaybe E.getArgs >>= getHandle
  setBuffering IO.stdout
  setBuffering h
  bufIn <- Buf.newByteBuffer chunkSize Buf.WriteBuffer
  bufOut <- Buf.newByteBuffer 1024 Buf.WriteBuffer
  Buf.withBuffer bufIn $ \bufferIn -> Buf.withBuffer bufOut $ \bufferOut -> do
    {-# SCC "encodeStream" #-} encodeStream 0 h bufferIn bufferOut
  where
    setBuffering handle = IO.hSetBuffering handle $ BlockBuffering $ Just $ 1024 * 64
    getHandle (Just filename) = IO.openFile filename IO.ReadMode
    getHandle Nothing = return IO.stdin
