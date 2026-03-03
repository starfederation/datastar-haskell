module Hypermedia.Datastar.PatchSignalsSpec (spec) where

import Test.Hspec

import Hypermedia.Datastar.PatchSignals
import Hypermedia.Datastar.Types

spec :: Spec
spec = describe "Hypermedia.Datastar.PatchSignals.toDatastarEvent" $ do
  it "minimal: single-line signals, all defaults" $ do
    let event = toDatastarEvent $ patchSignals "{\"output\":\"Patched Output Test\",\"show\":true}"

    eventType event `shouldBe` EventPatchSignals
    eventId event `shouldBe` Nothing
    retry event `shouldBe` defaultRetryDuration
    dataLines event `shouldBe` ["signals {\"output\":\"Patched Output Test\",\"show\":true}"]

  it "full: all options set" $ do
    let pOnlyIfMissing = True
        pEventId = Just "123"
        pRetryDuration = 2000

        ps =
          (patchSignals "{\"output\":\"Patched Output Test\",\"show\":true}")
            { psOnlyIfMissing = pOnlyIfMissing
            , psEventId = pEventId
            , psRetryDuration = pRetryDuration
            }
        event = toDatastarEvent ps

    eventType event `shouldBe` EventPatchSignals
    eventId event `shouldBe` pEventId
    retry event `shouldBe` pRetryDuration

    dataLines event
      `shouldBe` [ "onlyIfMissing true"
                 , "signals {\"output\":\"Patched Output Test\",\"show\":true}"
                 ]

  it "omits onlyIfMissing when false (default)" $ do
    let ps = patchSignals "{\"foo\":1}"
        event = toDatastarEvent ps
    dataLines event `shouldBe` ["signals {\"foo\":1}"]

  it "handles multi-line JSON" $ do
    let event = toDatastarEvent $ patchSignals "{\n  \"foo\": 1,\n  \"bar\": 2\n}"

    dataLines event
      `shouldBe` [ "signals {"
                 , "signals   \"foo\": 1,"
                 , "signals   \"bar\": 2"
                 , "signals }"
                 ]

  it "documents null removal via JSON merge patch" $ do
    let ps = patchSignals "{\"key\": null}"
        event = toDatastarEvent ps
    dataLines event `shouldBe` ["signals {\"key\": null}"]
