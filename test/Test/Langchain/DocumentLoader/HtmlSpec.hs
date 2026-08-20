{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.DocumentLoader.HtmlSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.DocumentLoader.Core (BaseLoader (..))
import Langchain.DocumentLoader.Html

tests :: TestTree
tests =
  testGroup
    "Langchain.DocumentLoader.HtmlSpec"
    [ testCase "extractCleanHtml extracts title and strips scripts/styles" $ do
        let rawHtml =
              "<html><head><title>My Test Page</title><style>body { color: red; }</style></head><body><h1>Hello World</h1><script>console.log('bad');</script><p>This is clean text.</p></body></html>"
            (mbTitle, cleanBody) = extractCleanHtml rawHtml
        mbTitle @?= Just "My Test Page"
        assertBool "Does not contain script code" (not $ "console.log" `TL.isInfixOf` cleanBody)
        assertBool "Does not contain style code" (not $ "color: red" `TL.isInfixOf` cleanBody)
        assertBool "Contains Hello World" ("Hello World" `TL.isInfixOf` cleanBody)
        assertBool "Contains clean text" ("This is clean text." `TL.isInfixOf` cleanBody)
    , testCase "HtmlLoader reads HTML file and extracts clean Document" $ do
        withSystemTempDirectory "html-loader-test" $ \tmpDir -> do
          let filePath = tmpDir </> "page.html"
              content = "<html><head><title>Doc Title</title></head><body><p>Paragraph content</p></body></html>"
          writeFile filePath content
          let loader = defaultHtmlLoader filePath
          res <- runExceptT $ load loader
          case res of
            Left err -> assertFailure ("HtmlLoader failed: " ++ show err)
            Right docs -> do
              length docs @?= 1
    ]
