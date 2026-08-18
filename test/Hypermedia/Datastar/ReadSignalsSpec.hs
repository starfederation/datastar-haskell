module Hypermedia.Datastar.ReadSignalsSpec (spec) where

import Test.Hspec

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)

import Network.Wai (defaultRequest, queryString, requestMethod)

import Hypermedia.Datastar.WAI (readSignals)

newtype Msg = Msg Text deriving (Eq, Show)

instance FromJSON Msg where
  parseJSON = withObject "Msg" $ \o -> Msg <$> o .: "msg"

spec :: Spec
spec = describe "readSignals" $ do
  let req q = defaultRequest{requestMethod = "GET", queryString = [("datastar", Just q)]}

  it "decodes JSON signals from the query string exactly as sent" $ do
    result <- readSignals (req "{\"msg\":\"a+b\"}")
    result `shouldBe` Right (Msg "a+b")
