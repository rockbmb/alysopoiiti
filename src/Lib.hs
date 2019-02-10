module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | Datatype representing a transaction input. Per the instructions, it
-- contains a numerical identifier and an amount. This datatype alone
-- does not enforce the identifier's uniqueness.
data TxIn = TxIn
    { id     :: !Integer
    , amount :: !Integer
    } deriving (Show, Eq)

-- | Datatype representing a transaction output. Per the instructions, it
-- contains a numerical identifier and an amount. This datatype alone
-- does not enforce the identifier's uniqueness.
data TxOut = TxOut
    { id     :: !Integer
    , amount :: !Integer
    } deriving (Show, Eq)

-- | Datatype representing a transaction. This datatype's constructors or
-- accessors alone do not enforce the requested property that the total
-- amount present in the inputs match the total amount present in the outputs.
data Transaction = Tx
    { inputs  :: [TxIn]
    , outputs :: [TxOut]
    } deriving (Show, Eq)