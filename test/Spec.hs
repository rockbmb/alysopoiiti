import Lib

import Data.Aeson (decode)

import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
    describe "Example JSON" $ do
        it "successfully decodes example JSON block" $
          (decode exampleJSON :: Maybe Block) == Just example
