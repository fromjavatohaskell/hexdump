{-# LANGUAGE OverloadedStrings #-}

-- license MIT https://raw.githubusercontent.com/fromjavatohaskell/hexdump/master/LICENSE-MIT

import           Data.Maybe ( listToMaybe )
import qualified System.Environment as E
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- E.getArgs
  d <- case listToMaybe args of
    Just filename -> BS.readFile filename
    Nothing -> BS.getContents
  BS.putStr d
