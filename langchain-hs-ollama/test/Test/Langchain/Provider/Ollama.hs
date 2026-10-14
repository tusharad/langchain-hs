{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Langchain.Provider.Ollama (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Langchain.Model
import Langchain.Provider.Ollama
import qualified Ollama.Client as OC
import qualified Ollama.Types.Format as OFormat
import qualified Ollama.Types.Message as O

testModelName :: Text
testModelName = "gemma3:latest"

-- | Skip live Ollama tests when running unit test suite
withOllamaModel :: Text -> (Text -> IO ()) -> IO ()
withOllamaModel _ _ = pure ()

data TestPerson = TestPerson
  { personName :: !Text
  , personAge :: !Int
  }
  deriving (Show, Eq, Generic, ToSchema)

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.Ollama"
    [ testCase "newOllama initializes provider with defaultConfig" $ do
        p <- newOllama testModelName defaultClientConfig
        ollamaModelName p @?= testModelName
    , testCase "newOllama accepts custom OllamaClientConfig" $ do
        let cfg =
              defaultClientConfig
                { configBaseUrl = "http://custom-host:11434"
                , configTimeout = 120
                }
        p <- newOllama "qwen3.5:2b" cfg
        ollamaModelName p @?= "qwen3.5:2b"
        configBaseUrl (OC.clientConfig (client p)) @?= "http://custom-host:11434"
        configTimeout (OC.clientConfig (client p)) @?= 120
    , testCase "newOllamaWithClient wraps existing OllamaClient" $ do
        c <- OC.defaultClient
        let p = newOllamaWithClient testModelName c
        ollamaModelName p @?= testModelName
    , testCase "defaultOllamaConfig is empty" $ do
        let cfg = defaultOllamaConfig
        ollamaTools cfg @?= Nothing
        ollamaFormat cfg @?= Nothing
        ollamaOptions cfg @?= Nothing
        ollamaKeepAlive cfg @?= Nothing
        ollamaThink cfg @?= Nothing
    , testCase "withOptions sets ModelOptions on OllamaConfig" $ do
        let opts = defaultOptions {optTemperature = Just 0.3, optNumCtx = Just 4096}
            cfg = withOptions opts defaultOllamaConfig
        case ollamaOptions cfg of
          Nothing -> assertFailure "Expected ollamaOptions in OllamaConfig"
          Just o -> do
            optTemperature o @?= Just 0.3
            optNumCtx o @?= Just 4096
    , testCase "withJsonFormat sets JsonFormat" $ do
        let cfg = withJsonFormat defaultOllamaConfig
        ollamaFormat cfg @?= Just OFormat.JsonFormat
    , testCase "withStructuredOutput sets SchemaFormat" $ do
        let cfg = withStructuredOutput (Proxy @TestPerson) defaultOllamaConfig
        case ollamaFormat cfg of
          Just (OFormat.SchemaFormat _) -> pure ()
          _ -> assertFailure "Expected SchemaFormat in OllamaConfig"
    , testCase "withKeepAlive sets keep-alive duration" $ do
        let cfg = withKeepAlive "10m" defaultOllamaConfig
        ollamaKeepAlive cfg @?= Just "10m"
    , testCase "toOllamaRole and fromOllamaRole map correctly" $ do
        toOllamaRole User @?= O.User
        toOllamaRole Assistant @?= O.Assistant
        toOllamaRole System @?= O.System
        toOllamaRole Tool @?= O.Tool
        toOllamaRole Developer @?= O.System
        toOllamaRole Function @?= O.Tool
        fromOllamaRole O.User @?= User
        fromOllamaRole O.Assistant @?= Assistant
        fromOllamaRole O.System @?= System
        fromOllamaRole O.Tool @?= Tool
    , testCase "toOllamaMessage and fromOllamaMessage roundtrip text" $ do
        let msg = userMessage "Hello Ollama"
            oMsg = toOllamaMessage msg
            backMsg = fromOllamaMessage oMsg
        messageRole backMsg @?= User
        extractMessageText backMsg @?= "Hello Ollama"
    , testCase "invoke returns Assistant message" $ do
        withOllamaModel testModelName $ \modelName -> do
          p <- newOllama modelName defaultClientConfig
          let input = [userMessage "What is 2 + 2? Answer with just the number."]
          res <- runExceptT $ invoke p input Nothing
          case res of
            Left err -> assertFailure $ "Expected success, got error: " ++ show err
            Right msg -> do
              messageRole msg @?= Assistant
              assertBool "Should contain 4" ("4" `T.isInfixOf` extractMessageText msg)
    , testCase "batch processes multiple inputs" $ do
        withOllamaModel testModelName $ \modelName -> do
          p <- newOllama modelName defaultClientConfig
          let inputs = [[userMessage "What is 1 + 1?"], [userMessage "What is 2 + 2?"]]
          res <- runExceptT $ batch p inputs Nothing
          case res of
            Left err -> assertFailure $ "Expected success, got error: " ++ show err
            Right msgs -> do
              length msgs @?= 2
    ]
