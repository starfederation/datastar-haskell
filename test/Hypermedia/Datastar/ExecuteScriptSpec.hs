module Hypermedia.Datastar.ExecuteScriptSpec (spec) where

import Test.Hspec

import Hypermedia.Datastar.ExecuteScript
import Hypermedia.Datastar.Types

spec :: Spec
spec = describe "Hypermedia.Datastar.ExecuteScript.toDatastarEvent" $ do
  it "minimal: single-line script with auto-remove" $ do
    let event = toDatastarEvent $ executeScript "console.log('Here')"

    eventType event `shouldBe` EventPatchElements
    eventId event `shouldBe` Nothing
    retry event `shouldBe` defaultRetryDuration
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\">console.log('Here')</script>"
                 ]

  it "full: all options set" $ do
    let pAutoRemove = True
        pAttrs = ["type=\"application/javascript\""]
        pEventId = Just "123"
        pRetryDuration = 2000
        es =
          (executeScript "console.log('Here')")
            { esAutoRemove = pAutoRemove
            , esAttributes = pAttrs
            , esEventId = pEventId
            , esRetryDuration = pRetryDuration
            }
        event = toDatastarEvent es

    eventType event `shouldBe` EventPatchElements
    eventId event `shouldBe` pEventId
    retry event `shouldBe` pRetryDuration
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\" type=\"application/javascript\">console.log('Here')</script>"
                 ]

  it "auto-remove disabled" $ do
    let es = (executeScript "alert(1)"){esAutoRemove = False}
        event = toDatastarEvent es
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script>alert(1)</script>"
                 ]

  it "multi-line script" $ do
    let es = executeScript "var x = 1;\nconsole.log(x);"
        event = toDatastarEvent es
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\">"
                 , "elements var x = 1;"
                 , "elements console.log(x);"
                 , "elements </script>"
                 ]

  it "multi-line script with three lines" $ do
    let es = executeScript "var x = 1;\nvar y = 2;\nconsole.log(x + y);"
        event = toDatastarEvent es
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\">"
                 , "elements var x = 1;"
                 , "elements var y = 2;"
                 , "elements console.log(x + y);"
                 , "elements </script>"
                 ]

  it "empty script" $ do
    let es = executeScript ""
        event = toDatastarEvent es
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\"></script>"
                 ]

  it "multiple attributes" $ do
    let es =
          (executeScript "go()")
            { esAttributes = ["type=\"module\"", "defer"]
            }
        event = toDatastarEvent es
    dataLines event
      `shouldBe` [ "selector body"
                 , "mode append"
                 , "elements <script data-effect=\"el.remove()\" type=\"module\" defer>go()</script>"
                 ]
