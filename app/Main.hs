{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Blockchain

import qualified Data.Aeson                 as A
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.IORef                           (IORef, newIORef, readIORef,
                                             writeIORef)
import System.IO.Unsafe                     (unsafePerformIO)

main :: IO ()
main = loop
  where

    -- The @NOINLINE@ is to avoid problems with @unsafePerformIO@.
    -- Note that @IORef@s are, for the most part, NOT thread-safe.
    ref :: IORef BlockTree
    {-# NOINLINE ref #-}
    ref = unsafePerformIO $ newIORef $ emptyTree

    output :: A.ToJSON a => a -> IO ()
    output = BSLC.putStrLn . A.encode

    -- Helper used to remove boilerplate when handling @Init@ or @Submit@
    -- commands - what to do if the command fails, and if it succeeds.
    handleResult :: (A.ToJSON out)
                 => Either CommandException BlockTree
                 -- ^ @Either@ value to be pattern matched
                 -> out
                 -- ^ Value to JSON-encode and write to @stdout@ if first
                 -- argument is @Right@.
                 -> IO ()
    handleResult res out = case res of
        Left err -> do output err
                       loop
        Right r  -> do output out
                       writeIORef ref r
                       loop

    -- This helper is like @handleResult@, but for the
    -- @QueryState@/@QueryHeads@ commands. @handleResult@ cannot be used
    --  because these commands do not use the value inside @Right@ to @loop@;
    -- in this case the client just reuses the data it had before to await new
    -- commands.
    handleResult' :: A.ToJSON a => Either CommandException a -> IO ()
    handleResult' res = case res of
        Left err -> do output err
                       loop
        Right query -> do output query
                          loop

    -- Main worker in the client. Note that a @BlockTree@ can only be
    -- initialized once - if it happens more than once, the binary prints
    -- that it has already been initialized, and awaits another command.
    loop :: IO ()
    loop = do
        -- As mentioned in the spec, the client expects newline-delimited JSON.
        -- It will not work otherwise.
        s <- BS.getLine
        case A.eitherDecode (BSL.fromStrict s) of
            Left err -> do putStrLn err
                           loop
            Right command -> do
                blocktree <- readIORef ref
                case command of
                    Init b -> handleResult (initChain b blocktree) OK
                    QueryState -> handleResult' (querySt blocktree)
                    QueryHeads -> handleResult' (queryHd blocktree)
                    Submit b -> handleResult (submitBlock b blocktree) OK