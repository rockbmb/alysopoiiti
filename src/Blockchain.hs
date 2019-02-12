{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blockchain
    ( Blockchain (..)
    , Blocktree
    , Command (..)
    , CommandException (..)
    , CommandResult
    , OK (..)
    , QuerySt (..)

    , initTree
    , querySt
    ) where

import Lib

import Data.Aeson                     (FromJSON (..), ToJSON (..), Value (..),
                                       object, pairs, (.=))
import Data.Aeson.Types               (typeMismatch)
import qualified Data.HashMap.Strict  as HMS
import Data.List                      (sortBy)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as MS
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

-- | Representation of a single blockchain (no forking).
data Blockchain
    = Genesis !Block
    | Mainchain !(NE.NonEmpty Block)

chainLength :: Blockchain -> Int
chainLength (Genesis _) = 1
chainLength (Mainchain (_ NE.:| l)) = 1 + length l

-- A "blocktree" can be @Map Hash Blockchain@, with @Blockchain@ the type
-- defined above.
-- In the above map, the key is the latest block's hash - allows for fast
-- retrieval of any given fork.
type Blocktree = MS.Map Hash Blockchain

initTree :: Block -> Either CommandException Blocktree
initTree b@Block {..} =
    let hash' = createHash b
        tree  = MS.singleton hash (Genesis b)
    in if hash' == hash then Right tree
                        else Left InitInvalidHashError

data QuerySt = QuerySt
    { height    :: !Int
    , stHash    :: !Hash
    , stOutputs :: ![TxOut]
    }
  deriving (Eq, Show)

-- | Query state command. In order to calculate current height, hash and
-- UTXO of the longest chain.
querySt :: Blocktree -> Either CommandException QuerySt
querySt m | MS.null m = Left QueryStUninitializedError
          | otherwise =
    let hashesAndChains = MS.toList m
        -- @sorted@ is a list of pairs of blockchains and the hash of their
        -- latest block, sorted is descension with the following order:
        -- 1. Compare the blockchains by length, choose the longest
        -- 2. If the lengths are the same, choose the chain whose latest
        --    block has the lexicographically greater hash.
        -- This is the rule used to choose forks.
        sorted :: [(Hash, Blockchain)]
        sorted = sortBy (\(h1, chain1) (h2, chain2) ->
            let res = compare (chainLength chain2) (chainLength chain1)
            in case res of
                EQ -> compare h1 h2
                _  -> res) hashesAndChains
        -- Because @m@ cannot be empty at this point, and @sortBy@ does not
        -- alter a list's length, @head@ is safe to use.
        (_, chain) = head sorted
    in case chain of
        Genesis Block {..} ->
            Right $ QuerySt 1 hash (concatMap outputs transactions)
        Mainchain (Block {..} NE.:| []) ->
            Left QueryStUninitializedError
        Mainchain (Block {..} NE.:| bs) -> Right $
            QuerySt (1 + length bs) hash (concatMap outputs transactions)