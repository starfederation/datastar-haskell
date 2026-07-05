module Hypermedia.Datastar.NegotiationSpec (spec) where

import Test.Hspec

import Data.ByteString.Lazy qualified as BL

import Network.Wai (defaultRequest, requestHeaders)

import Hypermedia.Datastar
import Hypermedia.Datastar.WAI (Compressor (..), negotiateWith)

{- | A compressor that only carries an encoding token, for negotiation tests.
Its 'compressorWrap' is never invoked by 'negotiateWith'.
-}
fakeCompressor :: BL.ByteString -> Compressor
fakeCompressor enc = Compressor (BL.toStrict enc) (\_ _ -> error "unused")

spec :: Spec
spec = describe "negotiateWith" $ do
  let br = fakeCompressor "br"
      gz = fakeCompressor "gzip"
      enc = fmap compressorEncoding
      req h = defaultRequest{requestHeaders = [("Accept-Encoding", h)]}

  it "ServerPriority picks the first server compressor the client accepts" $
    enc (negotiateWith ServerPriority [br, gz] (req "gzip, br")) `shouldBe` Just "br"

  it "ClientPriority picks the client's most-preferred offered encoding" $
    enc (negotiateWith ClientPriority [br, gz] (req "gzip, br")) `shouldBe` Just "gzip"

  it "Forced ignores Accept-Encoding entirely" $
    enc (negotiateWith Forced [br, gz] (req "identity")) `shouldBe` Just "br"

  it "ignores q-value params and whitespace around each token" $
    enc (negotiateWith ClientPriority [br, gz] (req "  gzip;q=0.1 , br ")) `shouldBe` Just "gzip"

  it "returns Nothing when no server encoding is accepted" $
    enc (negotiateWith ServerPriority [br, gz] (req "deflate")) `shouldBe` Nothing

  it "returns Nothing when the Accept-Encoding header is absent" $
    enc (negotiateWith ServerPriority [br, gz] defaultRequest) `shouldBe` Nothing
