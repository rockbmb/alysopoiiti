{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blockchain
    ( Command (..)
    , CommandException (..)
    , OK (..)
    ) where

import Lib

import Data.Aeson                     (FromJSON (..), ToJSON (..), Value (..),
                                       encode, object, pairs, withObject,
                                       withText, (.:), (.=))
import Data.Aeson.Types               (typeMismatch)
import qualified Data.HashMap.Strict  as HMS
import qualified Data.List.NonEmpty   as NE
import qualified Data.Text            as T

-- | Datatype to represent the different kinds of commands the client can
-- receive.
data Command
    = Init Block
    | QueryState
    | QueryHeads
    | Submit Block
  deriving (Show, Eq)

instance ToJSON Command where
    toJSON (Init b) = object ["init" .= toJSON b]
    toJSON QueryState = object ["query" .= String "state"]
    toJSON QueryHeads = object ["query" .= String "heads"]
    toJSON (Submit b) = object ["block" .= toJSON b]

    toEncoding (Init b) = pairs $ "init" .= toJSON b
    toEncoding QueryState = pairs $ "query" .= String "state"
    toEncoding QueryHeads = pairs $ "query" .= String "heads"
    toEncoding (Submit b) = pairs $ "block" .= toJSON b

instance FromJSON Command where
    parseJSON (Object o) = case HMS.toList o of
        [("init", block)] -> Init <$> parseJSON block
        [("query", qType)] -> case qType of
            String "state" -> pure QueryState
            String "heads" -> pure QueryHeads
            t              -> typeMismatch "Command.Query" t
        [("block", block)] -> Submit <$> parseJSON block
        l -> fail $ "Could not parse command: " ++ (T.unpack . T.unwords . map fst) l
    parseJSON invalid    = typeMismatch "Command" invalid

-- | Datatype enumerating, for each possible command, the error responses
-- that can be returned. This is important to differentiate the same error
-- but caused by different a command.
data CommandException
    = InitInvalidHashError
    | QueryStUninitializedError
    | QueryHdUninitializedError
    | SbmtUninitializedError
    | SbmtInvalidHashError
    | SbmtNoPredecessorError
    | SbmtDuplicateHashError
    | SbmtInvalidTxError

-- | This instance will be used to output a command's failure to stdout, in
-- JSON.
instance ToJSON CommandException where
    toJSON val = case val of
        InitInvalidHashError -> f invalidHashTxt
        QueryStUninitializedError -> f uninitializedTxt
        QueryHdUninitializedError -> f uninitializedTxt
        SbmtUninitializedError -> f uninitializedTxt
        SbmtInvalidHashError -> f invalidHashTxt
        SbmtNoPredecessorError -> f "no predecessor found"
        SbmtDuplicateHashError -> f "duplicate hash"
        SbmtInvalidTxError -> f "invalid transaction"
      where
        err = "error"
        invalidHashTxt = String "invalid hash"
        uninitializedTxt = String "must initialize first"
        f txt = object [(err, txt)]

-- | Datatype to represent the client's reply in case of a successful command.
data OK = OK

instance ToJSON OK where
    toJSON OK = object [("ok", Null)]

-- | Datatype to represent the result, success or failure, of a command made
-- to the client.
type CommandResult = Either CommandException OK

-- A blockchain can be @Map Hash Blockchain@, with @Blockchain = NonEmpty Blockchain@.
-- In the map, the hash key is the latest block's hash - allows for fast retrieval of
-- any given fork.

-- | Representation of a single blockchain (no forking).
data Blockchain
    = Genesis Block
    | Mainchain (NE.NonEmpty Block)

init :: Block -> Either CommandException Blockchain
init b@Block {..} =
    let hash' = createHash b
    in if hash' == hash then Right (Genesis b)
                        else Left InitInvalidHashError

data QuerySt = QuerySt
    { height    :: Int
    , stHash    :: Hash
    , stOutputs :: [TxOut]
    }
  deriving (Eq, Show)

querySt :: Blockchain -> QuerySt
querySt (Genesis Block {..}) = QuerySt 1 hash (concatMap outputs transactions)
querySt (Mainchain (Block {..} NE.:| _)) = undefined