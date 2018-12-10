{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.Int ( Int64 )
import           Data.ByteString.Lazy ( ByteString )
import           Data.ByteString.Builder ( Builder )
import           Data.Maybe ( listToMaybe )
import           Control.Monad ( void )
import qualified System.Environment as E
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

toBuilder :: Chunk -> Builder
toBuilder (Chunk _ offset chunk _) = B.int64HexFixed offset <> B.char8 ' ' <> B.lazyByteStringHex chunk <> B.char8 '\n'

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BSL.readFile filename
    Nothing -> BSL.getContents
  IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 16
  void $ traverse (B.hPutBuilder IO.stdout . toBuilder) (split d)
