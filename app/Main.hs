module Main where

import Lib

import qualified Data.ByteString as BS

main :: IO ()
main = BS.interact id
