{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blockchain
    ( Blockchain
    , BlockTree (..)
    , Command (..)
    , CommandException (..)
    , CommandResult
    , OK (..)
    , QuerySt (..)

    , emptyTree
    , initChain
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
-- kind. This assumption is not verified.
type Blockchain = NE.NonEmpty Block

-- A "blocktree" is a datatype that contains:
-- * A set of hashes of blocks that are currently childless - these are
-- the heads of all the existing chains.
-- * A map whose keys are every hash that has been accepted in any chain,
--   and whose values are the block represented by said hash.
-- * A map of hashes of blocks to the hashes of the block that precedes them.
--
-- This representation is an improvement over the previous one in terms of
-- space complexity because chains are no longer stored, only blocks, and each
-- block is only stored once, compared to an unlimited amount of time as it was
-- before. The downside is that in order to calculate the chain that ends at a
-- particular block, it's necessary to traverse it from end to start to rebuild
-- it, which has a penalty in time complexity that the previous implementation
-- did not have.
--
-- This type relies on the assumption that the SHA256 hash function is
-- injective. In theory it's not (as most random oracles aren't), but in
-- practice it is since it's EXTREMELY unlikely the number of total blocks
-- from every fork will exceed the square root of the number of possible
-- outputs.
--
-- This datatype has some invariants:
-- 1. Once a block becomes another's predecessors, it is removed from the
--    set of heads, and will NEVER be added to it again.
-- 2. Once blocks are added to the map from their hashes to themselves, they
--    are NEVER removed, and they are only ever inserted there ONCE. This
--    means this map MUST always be a bijective function from hashes to their
--    blocks.
-- 3. Genesis blocks NEVER have predecessors.
--
-- Note that with this implementation, a tree can be initialized many times -
-- each time it is, a new genesis block is created from which more chains can
-- be built.
data BlockTree = BlockTree
    { heads        :: !(S.Set Hash)
    -- ^ Hashes that represent blocks which are currently at the tip of their
    -- respective chains.
    , blocks       :: !(MS.Map Hash Block)
    -- ^ Map from each hash to the block it represents, and the hash of the
    -- block that predeces it.
    , predecessors :: !(MS.Map Hash Hash)
    -- ^ Map from a block's hash to the hash of its predecessor. Note that
    -- since genesis blocks don't have predecessors, they won't ever be keys
    -- in this map.
    }

emptyTree :: BlockTree
emptyTree = BlockTree mempty mempty mempty

-- | Given a block and a @BlockTree@, construct the blockchain whose tip
-- is the given block.
toChain :: Block -> BlockTree -> Blockchain
toChain block BlockTree {..} = toChain' block (block NE.:| [])
  where
    toChain' :: Block -> Blockchain -> Blockchain
    toChain' Block {..} !acc =
        case MS.lookup hash predecessors of
            Nothing -> acc
            Just predHash -> case MS.lookup predHash blocks of
                Nothing -> error "toChain: Block has a predecessor in map of\
                                 \ hashes but none in map of blocks - not\
                                 \ supposed to happen."
                Just predBlock -> toChain' predBlock (NE.cons predBlock acc)

isEmpty :: BlockTree -> Bool
isEmpty BlockTree {..} = MS.null blocks

-- | Initializes a new chain in the block tree by adding its genesis block.
-- Note that this can be done an undefinite amount of times, meaning there
-- can be many genesis blocks.
initChain :: Block -> BlockTree -> Either CommandException BlockTree
initChain b@Block {..} BlockTree {..} =
    let hash' = createHash b
        heads' = S.insert hash heads
        blocks' = MS.insert hash b blocks
    in if hash' == hash then Right $ BlockTree heads' blocks' predecessors
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
querySt bt@BlockTree {..} | MS.null blocks = Left QueryStUninitializedError
                          | otherwise =
    let -- Only consider chains that end in a head i.e. a block with no
        -- children.
        hashesAndChains :: [(Hash, Blockchain)]
        hashesAndChains = map (\(h, b) -> (h, toChain b bt)) $
                          MS.toList $
                          MS.restrictKeys blocks heads
        -- @sorted@ is a list of pairs of blockchains and the hash of their
        -- latest block, sorted is descension with the following order:
        -- 1. Compare the blockchains by length, choose the longest
        -- 2. If the lengths are the same, choose the chain whose latest
        --    block has the lexicographically greater hash.
        -- This is the rule used to choose forks.
        sorted :: [(Hash, Blockchain)]
        sorted = sortBy (\(h1, chain1) (h2, chain2) ->
            let res = compare (NE.length chain2) (NE.length chain1)
            in case res of
                EQ -> compare h1 h2
                _  -> res) hashesAndChains
        -- Because @sortBy@ does not alter a list's length, @head@ is safe to
        -- use (assuming the invariant holds that @heads@ is never empty).
        (_, Block {..} NE.:| bs) = head sorted
    in Right $ QuerySt (1 + length bs) hash (concatMap outputs transactions)

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

-- | Query heads command. Returns the current height and hash and UTXO of the
-- every chain.
queryHd :: BlockTree -> Either CommandException QueryHds
queryHd bt@BlockTree {..} | MS.null blocks = Left QueryHdUninitializedError
                       | otherwise =
    let f :: Blockchain -> QueryHd
        f x@(Block {..} NE.:| _) = QueryHd (NE.length x) hash
       -- The only chains that are going to matter for this command are those
       -- that end in childless blocks (heads).
    in Right .
       QueryHds .
       fmap (f . flip toChain bt) .
       MS.elems $ MS.restrictKeys blocks heads

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
    | MS.null blocks = Left SbmtUninitializedError
    | MS.member hash blocks = Left SbmtDuplicateHashError
    | any (not . isTxValid) transactions = Left SbmtInvalidTxError
    | not (MS.member predecessor blocks) = Left SbmtNoPredecessorError
    | createHash block /= hash = Left SbmtInvalidHashError
    | otherwise =
        let heads'        = S.insert hash $ S.delete predecessor heads
            blocks'       = MS.insert hash block blocks
            predecessors' = MS.insert hash predecessor predecessors
        in Right $ BlockTree heads' blocks' predecessors'