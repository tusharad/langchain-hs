{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.Core.Tool (tests) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import Langchain.Core.Error
import Langchain.Core.Tool
import Test.Tasty
import Test.Tasty.HUnit

calcExec :: Value -> IO (Either LangchainError Text)
calcExec (Object _) = pure $ Right "42"
calcExec _ = pure $ Left $ toolError "Invalid arguments" (Just "calc") Nothing

tests :: TestTree
tests =
  testGroup
    "Langchain.Core.Tool"
    [ testCase "createTool initializes tool attributes" $ do
        let t = createTool "calculator" "Performs math" (object []) calcExec
        toolName t @?= "calculator"
        toolDescription t @?= "Performs math"
    , testCase "toolToValue generates correct JSON schema structure" $ do
        let t = createTool "calculator" "Performs math" (object ["type" .= ("object" :: Text)]) calcExec
            val = toolToValue t
        case val of
          Object o -> do
            case parseEither (.: "type") o of
              Right ("function" :: Text) -> pure ()
              res -> assertFailure $ "Expected type = function, got: " ++ show res
          _ -> assertFailure "Expected JSON Object"
    , testCase "toolExecute runs successfully on valid input" $ do
        let t = createTool "calculator" "Performs math" (object []) calcExec
        res <- toolExecute t (object [])
        res @?= Right "42"
    ]
