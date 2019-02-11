{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Block (..)
    , Hash
    , Transaction (..)
    , TxIn (..)
    , TxOut (..)

    , createHash
    , example
    , exampleJSON
    , someFunc
    ) where

-- Unqualified imports (or exports) are not optimal, they pollute the module
-- namespace and obfuscate what is in it, what it uses, and what is makes
-- public.
import qualified Crypto.Hash          as C
import Data.Aeson                     (FromJSON (..), ToJSON (..), Value (..),
                                       encode, object, pairs, withObject,
                                       withText, (.:), (.=))
import Data.List                      (sortOn)
import Data.ByteArray                 (convert)
import Data.ByteArray.Encoding        (Base (..), convertToBase)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import Data.Text.Encoding             (decodeUtf8, encodeUtf8)
import GHC.Generics                   (Generic)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Datatype representing a transaction input. Per the instructions, it
-- contains a numerical identifier and an amount. This datatype alone
-- does not enforce the identifier's uniqueness.
data TxIn = TxIn
    { inId     :: !Integer
    , inAmount :: !Integer
    } deriving (Show, Eq)

instance ToJSON TxIn where
    toJSON (TxIn {..}) =
        object ["amount" .= inAmount, "id" .= inId]

instance FromJSON TxIn where
    parseJSON = withObject "TxIn" $ \v -> TxIn
        <$> v .: "in"
        <*> v .: "amount"

-- | Datatype representing a transaction output. Per the instructions, it
-- contains a numerical identifier and an amount. This datatype alone
-- does not enforce the identifier's uniqueness.
data TxOut = TxOut
    { outId     :: !Integer
    , outAmount :: !Integer
    } deriving (Show, Eq)

instance ToJSON TxOut where
    toJSON (TxOut {..}) =
        object ["amount" .= outAmount, "id" .= outId]

instance FromJSON TxOut where
    parseJSON = withObject "TxOut" $ \v -> TxOut
        <$> v .: "in"
        <*> v .: "amount"

-- | Datatype representing a transaction. This datatype's constructors or
-- accessors alone do not enforce the requested property that the total
-- amount present in the inputs match the total amount present in the outputs.
data Transaction = Tx
    { inputs  :: ![TxIn]
    , outputs :: ![TxOut]
    } deriving (Show, Eq)

instance ToJSON Transaction where
    toJSON (Tx {..}) =
        object ["inputs" .= inputs, "outputs" .= outputs]

instance FromJSON Transaction where
    parseJSON = withObject "Tx" $ \v -> Tx
        <$> v .: "inputs"
        <*> v .: "outputs"

-- | This type exists to separate hashing implementation details from the
-- actuall @Block@ datatype implementation.
-- It's a newtype to avoid using a @ByteString@ that isn't actually a hash
-- as one.
newtype Hash = Hash BS.ByteString deriving (Show, Eq)

instance ToJSON Hash where
    toJSON (Hash bs) = String $ decodeUtf8 bs

instance FromJSON Hash where
    parseJSON = withText "Hash" (pure . Hash . encodeUtf8)

-- | Datatype used to represent a single block.
data Block = Block
    { predecessor  :: !Hash
    , transactions :: ![Transaction]
    , hash         :: !Hash
    } deriving (Show, Eq)

instance ToJSON Block where
    toJSON (Block {..}) =
        object [ "predecessor" .= predecessor
               , "transactions" .= transactions
               , "hash" .= hash]

    toEncoding (Block {..}) =
        pairs $ "predecessor" .= predecessor
             <> "transactions" .= transactions
             <> "hash" .= hash

instance FromJSON Block where
    parseJSON = withObject "Block" $ \v -> Block
        <$> v .: "predecessor"
        <*> v .: "transactions"
        <*> v .: "hash"

-- | Given a block, create a hash of its contents according to the
-- specification.
-- Even if the result is always a 256-bit @ByteString@, this function performs
-- some costly conversion operations (see comments below), so its performance
-- when called in bulk will not be very high.
createHash :: Block -> Hash
createHash Block {..} =
    let encoded :: BSL.ByteString
        encoded = encode (predecessor, transactions)
    in (Hash .
        BS.append "0x" .
        -- I prefer this roundtrip conversion to @Text@ is than to use @String@,
        -- or even to manipulating individual @ByteString@ characters.
        -- It is not optimal, but it is simpler.
        encodeUtf8 .
        T.toLower .
        decodeUtf8 .
        --
        convertToBase Base16 .
        (C.hash :: BS.ByteString -> C.Digest C.SHA256) .
        BSL.toStrict) $ encoded

-- | This example block is just to verify block JSON encoding works as
-- expected. It's simplistic but in the absence of more complex testing
-- (i.e.) it's useful to check everything works as intended.
example :: Block
example = Block
    { predecessor  = Hash "0xaa5598c670c7f9c9ab8594d942390dad96b52631bfb78d060476b9a719365947"
    , transactions = [Tx [TxIn 2 7] [TxOut 3 4, TxOut 4 3],
                                     Tx [TxIn 13 15, TxIn 34 15] [TxOut 73 30]
                                    ]
    , hash         = Hash "0x264dd6805ff995edec60cb4af1e06984d37e0f987adb4851437c216c9dc3901f"
    }

exampleJSON :: BSL.ByteString
exampleJSON = encode example