{-# LANGUAGE OverloadedStrings #-}


import Control.Monad (void)
import System.Environment as E
import System.IO as IO

main :: IO ()
main = do
  args <- E.getArgs
  void $ traverse putStrLn args
