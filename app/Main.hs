{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blockchain
import Lib                                  ()

import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

main :: IO ()
main = loop (BlockTree mempty mempty)
  where
    output :: A.ToJSON a => a -> IO ()
    output = BSLC.putStrLn . A.encode

    loop :: BlockTree -> IO ()
    loop m = do
        s <- BS.getLine
        case A.eitherDecode (BSL.fromStrict s) of
            Left err -> do putStr err
                           loop m
            Right command -> case command of
                Init b -> if isEmpty m
                    then case initTree b of
                        Left err -> do output err
                                       loop m
                        Right m' -> do output OK
                                       loop m'
                    else do BSL.putStr "Chain has already been initialized"
                            loop m
                QueryState -> case querySt m of
                    Left err -> do output err
                                   loop m
                    Right qst -> do output qst
                                    loop m
                QueryHeads -> case queryHd m of
                    Left err -> do output err
                                   loop m
                    Right qhd -> do output qhd
                                    loop m
                Submit b -> case submitBlock b m of
                    Left err -> do output err
                                   loop m
                    Right m' -> do output OK
                                   loop m'