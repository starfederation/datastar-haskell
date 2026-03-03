module Hypermedia.Datastar.PatchElementsSpec where

import Data.Text qualified as T
import Test.Hspec

import Hypermedia.Datastar
import Hypermedia.Datastar.PatchElements
import Hypermedia.Datastar.Types

spec :: Spec
spec = describe "Hypermedia.Datastar.PatchElements.toDatastarEvent" $ do
  it "minimal: single-line elements, all defaults" $ do
    let event =
          toDatastarEvent $
            patchElements "<div id=\"feed\"><span>1</span></div>"

    eventType event `shouldBe` EventPatchElements
    eventId event `shouldBe` Nothing
    retry event `shouldBe` defaultRetryDuration
    dataLines event `shouldBe` ["elements <div id=\"feed\"><span>1</span></div>"]

  it "full: all options set" $ do
    let event =
          toDatastarEvent $
            (patchElements "<div id=\"feed\">\n    <span>1</span>\n</div>")
              { peSelector = Just "#feed"
              , peMode = Inner
              , peUseViewTransition = True
              , peNamespace = HtmlNs
              , peEventId = Just "123"
              , peRetryDuration = 2000
              }

    eventType event `shouldBe` EventPatchElements
    eventId event `shouldBe` Just "123"
    retry event `shouldBe` 2000
    dataLines event
      `shouldBe` [ "selector #feed"
                 , "mode inner"
                 , "useViewTransition true"
                 , "elements <div id=\"feed\">"
                 , "elements     <span>1</span>"
                 , "elements </div>"
                 ]

  it "remove: mode remove with selector, no elements" $ do
    let pe = removeElements "#feed, #otherid"
        event = toDatastarEvent pe
    dataLines event
      `shouldBe` [ "selector #feed, #otherid"
                 , "mode remove"
                 ]

  it "append with selector" $ do
    let pe =
          (patchElements "<div>New content</div>")
            { peSelector = Just "#mycontainer"
            , peMode = Append
            }
        event = toDatastarEvent pe
    dataLines event
      `shouldBe` [ "selector #mycontainer"
                 , "mode append"
                 , "elements <div>New content</div>"
                 ]

  it "SVG namespace" $ do
    let pe =
          (patchElements "<circle id=\"c1\" cx=\"10\" r=\"5\" fill=\"red\"/>\n<circle id=\"c2\" cx=\"20\" r=\"5\" fill=\"green\"/>\n<circle id=\"c3\" cx=\"30\" r=\"5\" fill=\"blue\"/>")
            { peSelector = Just "#vis"
            , peMode = Append
            , peNamespace = SvgNs
            }
        event = toDatastarEvent pe
    dataLines event
      `shouldBe` [ "selector #vis"
                 , "mode append"
                 , "namespace svg"
                 , "elements <circle id=\"c1\" cx=\"10\" r=\"5\" fill=\"red\"/>"
                 , "elements <circle id=\"c2\" cx=\"20\" r=\"5\" fill=\"green\"/>"
                 , "elements <circle id=\"c3\" cx=\"30\" r=\"5\" fill=\"blue\"/>"
                 ]

  it "omits namespace when html (default)" $ do
    let lines =
          dataLines $
            toDatastarEvent $
              patchElements "<p>hello</p>"

    any (T.isPrefixOf "namespace") lines `shouldBe` False

  it "omits mode when Outer (default)" $ do
    let pe = patchElements "<p>hello</p>"
        event = toDatastarEvent pe
        lines = dataLines event
    any (T.isPrefixOf "mode") lines `shouldBe` False

  it "politely ignores empty element strings" $ do
    let lines =
          dataLines $
            toDatastarEvent $
              patchElements ""
    lines `shouldBe` []
