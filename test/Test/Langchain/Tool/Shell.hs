{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.Shell (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Core.Tool (toolExecute)
import Langchain.Tool.Shell (shellTool)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.Shell"
    [ testCase "shellTool executes echo command correctly" $ do
        res <- toolExecute shellTool (object ["command" .= ("echo 'hello shell'" :: Text)])
        case res of
          Left err -> assertFailure ("shellTool failed: " ++ show err)
          Right out -> out @?= "hello shell"
    , testCase "shellTool handles non-zero exit code without crash" $ do
        res <- toolExecute shellTool (object ["command" .= ("exit 2" :: Text)])
        case res of
          Left err -> assertFailure ("shellTool failed with error: " ++ show err)
          Right out -> assertBool "Contains exit code" ("exited with code" `T.isInfixOf` out)
    ]
