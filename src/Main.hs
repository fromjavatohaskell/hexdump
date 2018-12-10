{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.Int ( Int64 )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Builder ( Builder )
import           Data.Maybe ( listToMaybe )
import           Control.Monad ( void )
import qualified System.Environment as E
import qualified Data.ByteString as BS hiding (scanl')
import qualified Data.ByteString.Lazy as BSL hiding (scanl')
import qualified Data.ByteString.Builder as BS
import           System.IO ( BufferMode(..) )
import qualified System.IO as IO

data Chunk = Chunk !Bool {-# UNPACK #-} !Int64 !ByteString !L.ByteString

chunkSize :: Int64
chunkSize = 16

initialChunk :: L.ByteString -> Chunk
initialChunk x = f $ BSL.splitAt chunkSize x
  where f (a, b) = Chunk True 0 (BSL.toStrict a) b

nextChunk :: Chunk -> Chunk
nextChunk (Chunk ne offset head rest) = f $ BSL.splitAt chunkSize rest
  where f (a, b) = Chunk (ne && prevNonEmpty) (offset + fromIntegral (BS.length head)) (BSL.toStrict a) b 
        prevNonEmpty = not $ BS.null head

notEmpty :: Chunk -> Bool
notEmpty (Chunk ne _ head _) = ne

split :: L.ByteString -> [Chunk]
split x = takeWhile notEmpty $ iterate nextChunk $ initialChunk x

toBuilder :: Chunk -> Builder
toBuilder (Chunk _ offset chunk _) = BS.int64HexFixed offset <> BS.char8 ' ' <> BS.byteStringHex chunk <> BS.char8 '\n'

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BSL.readFile filename
    Nothing -> BSL.getContents
  IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 16
  void $ traverse (BS.hPutBuilder IO.stdout . toBuilder) (split d)
