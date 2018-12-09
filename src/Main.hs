{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.ByteString ( ByteString )
import           Data.ByteString.Builder ( Builder )
import           Data.Maybe ( listToMaybe )
import           Control.Monad ( void )
import qualified System.Environment as E
import qualified Data.ByteString as BS hiding (scanl')
import qualified Data.ByteString.Builder as BS
import           System.IO ( BufferMode(..) )
import qualified System.IO as IO

data Chunk = Chunk !Bool {-# UNPACK #-} !Int !ByteString !ByteString

chunkSize :: Int
chunkSize = 16

initialChunk :: ByteString -> Chunk
initialChunk x = f $ BS.splitAt chunkSize x
  where f (a, b) = Chunk True 0 a b

nextChunk :: Chunk -> Chunk
nextChunk (Chunk ne offset head rest) = f $ BS.splitAt chunkSize rest
  where f (a, b) = Chunk (ne && prevNonEmpty) (offset + BS.length head) a b 
        prevNonEmpty = not $ BS.null head

notEmpty :: Chunk -> Bool
notEmpty (Chunk ne _ head _) = ne

split :: ByteString -> [Chunk]
split x = takeWhile notEmpty $ iterate nextChunk $ initialChunk x

toBuilder :: Chunk -> Builder
toBuilder (Chunk _ offset chunk _) = BS.intDec offset <> BS.char8 ' ' <> BS.byteStringHex chunk <> BS.char8 '\n'

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BS.readFile filename
    Nothing -> BS.getContents
  IO.hSetBuffering IO.stdout $ BlockBuffering $ Just $ 1024 * 16
  void $ traverse (BS.hPutBuilder IO.stdout . toBuilder) (split d)
