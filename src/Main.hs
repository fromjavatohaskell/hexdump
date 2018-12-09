{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.ByteString ( ByteString )
import           Data.Maybe ( listToMaybe )
import           Control.Monad ( void )
import qualified System.Environment as E
import qualified Data.ByteString as BS hiding (scanl')

data Chunk = Chunk {-# UNPACK #-} !Int !ByteString !ByteString

chunkSize :: Int
chunkSize = 16

initialChunk :: ByteString -> Chunk
initialChunk x = f $ BS.splitAt chunkSize x
  where f (a, b) = Chunk 0 a b

nextChunk :: Chunk -> Chunk
nextChunk (Chunk offset head rest) = f $ BS.splitAt chunkSize rest
  where f (a, b) = Chunk (offset + BS.length head) a b 

notEmpty :: Chunk -> Bool
notEmpty (Chunk _ head _) = not $ BS.null head

split :: ByteString -> [Chunk]
split x = takeWhile notEmpty $ iterate nextChunk $ initialChunk x

instance Show Chunk where
  show = showChunk

showChunk (Chunk offset chunk _) = show offset

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BS.readFile filename
    Nothing -> BS.getContents
  void $ traverse (putStrLn . show) (split d)
