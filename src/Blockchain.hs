{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blockchain
    ( Blockchain (..)
    , BlockTree (..)
    , Command (..)
    , CommandException (..)
    , CommandResult
    , OK (..)
    , QuerySt (..)

    , initTree
    , isEmpty
    , queryHd
    , querySt
    , submitBlock
    ) where

import Lib

import Data.Aeson                     (FromJSON (..), ToJSON (..), Value (..),
                                       object, pairs, (.=))
import Data.Aeson.Types               (typeMismatch)
import qualified Data.HashMap.Strict  as HMS
import Data.List                      (sortBy)
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map.Strict      as MS
import qualified Data.Set             as S
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

-- | Representation of a single blockchain (no forking). This datatype
-- assumes that if a chain only has one block, then said block is the genesis
-- kind. This assumtpion is not verified.
data Blockchain = Chain !(NE.NonEmpty Block)

chainLength :: Blockchain -> Int
chainLength (Chain (_ NE.:| l)) = 1 + length l

-- A "blocktree" is a datatype that represents:
-- * A map whose keys are every block that has been accepted in any chain, the
--   and whose values are the chain that precedes said block.
-- * A set of hashes of blocks that are currently childless - these are
-- the heads of all the existing chains.
-- In the above map, the key is the latest block's hash - allows for fast
-- retrieval of any given fork.
--
-- This is an inefficient way of representing forked chains because in most
-- cases, there is a lot of sharing - several forks might only differ in the last
-- few blocks, but each will index the entire chain, effectively duplicating
-- it.
--
-- It's possible to fix this by instead storing every block in a set, have
-- a map keeping track of which block precedes which, and have the set of
-- heads remain the same. The main tradeoff of this approach compared to the
-- current one is that everytime a certain chain is needed, it has to be
-- reconstructed, whereas now, although space complexity is poor, any chain
-- is available on demand.
--
-- This type relies on the assumption that the SHA256 hash function is
-- injective. In theory it's not (as most random oracles aren't), but in
-- practice it is since it's EXTREMELY unlikely the number of total blocks
-- from every fork will exceed the square root of the number of possible
-- outputs.
data BlockTree = BlockTree
    { heads  :: !(S.Set Hash)
    , chains :: !(MS.Map Hash Blockchain)
    }

isEmpty :: BlockTree -> Bool
isEmpty BlockTree {..} = MS.null chains

initTree :: Block -> Either CommandException BlockTree
initTree b@Block {..} =
    let hash' = createHash b
        heads = S.singleton hash
        chains = MS.singleton hash (Chain $ return b)
    in if hash' == hash then Right BlockTree {..}
                        else Left InitInvalidHashError

-- | This datatype is to represent the result of a @QueryState@ command.
data QuerySt = QuerySt
    { stHeight  :: !Int
    , stHash    :: !Hash
    , stOutputs :: ![TxOut]
    }
  deriving (Eq, Show)

instance ToJSON QuerySt where
    toJSON (QuerySt {..}) = object [ "height" .= stHeight
                                   , "hash" .= stHash
                                   , "outputs" .= stOutputs]

    toEncoding (QuerySt {..}) = pairs $  "height" .= stHeight
                                      <> "hash" .= stHash
                                      <> "outputs" .= stOutputs

-- | Query state command. Returns the current height, hash and UTXO of the
-- longest chain.
-- This function's performance may be improved if chain length information
-- is stored along with each chain, which may require a different data
-- structure (perhaps a priority search queue where the priority is the
-- length).
querySt :: BlockTree -> Either CommandException QuerySt
querySt BlockTree {..} | MS.null chains = Left QueryStUninitializedError
                       | otherwise =
    let -- Only consider chains that end in a head i.e. a block with no
        -- children.
        hashesAndChains :: [(Hash, Blockchain)]
        hashesAndChains = MS.toList $ MS.restrictKeys chains heads
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
        Chain (Block {..} NE.:| bs) -> Right $
            QuerySt (1 + length bs) hash (concatMap outputs transactions)

data QueryHd = QueryHd
    { hdHeight  :: !Int
    , hdHash    :: !Hash
    }
  deriving (Eq, Show)

instance ToJSON QueryHd where
  toJSON (QueryHd {..}) = object [ "height" .= hdHeight
                                 , "hash" .= hdHash]

  toEncoding (QueryHd {..}) = pairs $  "height" .= hdHeight
                                    <> "hash" .= hdHash

-- | This datatype is to represent the result of a @QueryHead@ command.
data QueryHds = QueryHds [QueryHd]
  deriving (Eq, Show)

instance ToJSON QueryHds where
    toJSON (QueryHds hds) = object ["heads" .= toJSON hds]

-- | Get a chain's latest hash.
latestHash :: Blockchain -> Hash
latestHash (Chain (Block {..} NE.:| _)) = hash

-- | Query heads command. Returns the current height and hash and UTXO of the
-- every chain.
queryHd :: BlockTree -> Either CommandException QueryHds
queryHd BlockTree {..} | MS.null chains = Left QueryHdUninitializedError
                       | otherwise =
    let f :: Blockchain -> QueryHd
        f x = QueryHd (chainLength x) (latestHash x)
       -- The only chains that are going to matter for this command are those
       -- that end in childless blocks (heads).
    in Right . QueryHds . fmap (f . (chains MS.!)) . S.toList $ heads

-- | Checks whether or not a given transaction is valid. Reminder that it is
-- valid iff the total amount in its inputs sums to the same as the total
-- amount in its outputs.
isTxValid :: Transaction -> Bool
isTxValid Tx {..} =
    (sum . map inAmount) inputs == (sum . map outAmount) outputs

-- | Adds a block to a blocktree. As the other functions above, it uses
-- @Either CommandException@ to signal possible failure.
submitBlock :: Block -> BlockTree -> Either CommandException BlockTree
submitBlock block@(Block {..}) BlockTree {..}
    | MS.null chains = Left SbmtUninitializedError
    | MS.member hash chains = Left SbmtDuplicateHashError
    | any (not . isTxValid) transactions = Left SbmtInvalidTxError
    | not (MS.member predecessor chains) = Left SbmtNoPredecessorError
    | createHash block /= hash = Left SbmtInvalidHashError
    | otherwise =
        let chain  = chains MS.! predecessor
            heads' = S.insert hash $ S.delete predecessor heads
            -- This small helper doesn't check the integrity of anything,
            -- that's done above.
            cons :: Block -> Blockchain -> Blockchain
            cons b (Chain c) = Chain $ NE.cons b c
            chain' = cons block chain
            m''    = BlockTree heads' $ MS.insert hash chain' chains
        in Right m''

-- DO NOT FORGET TO REFACTOR:
-- * MAIN (USE AN IOREF FOR THE BLOCKTREE?)