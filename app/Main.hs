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

    -- Helper used to remove boilerplate when handling @Init@ or @Submit@
    -- commands - what to do if the command fails, and if it succeeds.
    handleResult :: (A.ToJSON out)
                 => Either CommandException a
                 -- ^ @Either@ value to be pattern matched
                 -> a
                 -- ^ Value to perform @IO@ action with if above argument is
                 -- @Left@
                 -> out
                 -- ^ Value to JSON-encode and write to @stdout@ if first
                 -- argument is @Right@.
                 -> (a -> IO ())
                 -- ^ @IO@ action to take after using @output@ to write to
                 -- @stdout@.
                 -> IO ()
    handleResult res left out go = case res of
        Left err -> do output err
                       go left
        Right r  -> do output out
                       go r

    -- This helper is like @handleResult@, but for the
    -- @QueryState@/@QueryHeads@ commands. @handleResult@ cannot be used
    --  because these commands do not use the value inside @Right@ to @loop@;
    -- in this case the client just reuses the data it had before to await new
    -- commands.
    handleResult' :: (A.ToJSON out)
                  => Either CommandException a -> IO () -> IO ()
    handleResult' res go = case res of
        Left err -> do output err
                       go
        Right query -> do output query
                          go

    -- Main worker in the client. To note that a @BlockTree@ can only be
    -- initialized once - if it happens more than once, the binary prints
    -- that it has already been initialized, and awaits another command.
    loop :: BlockTree -> IO ()
    loop m = do
        -- As mentioned in the spec, the client expects newline-delimited JSON.
        -- It will not work otherwise.
        s <- BS.getLine
        case A.eitherDecode (BSL.fromStrict s) of
            Left err -> do putStrLn err
                           loop m
            Right command -> case command of
                Init b -> if isEmpty m
                    then handleResult (initTree b) m OK loop
                    else do BSLC.putStrLn "Chain has already been initialized"
                            loop m
                QueryState -> handleResult' (querySt m) (loop m)
                QueryHeads -> handleResult' (queryHd m) (loop m)
                Submit b -> handleResult (submitBlock b m) m OK loop