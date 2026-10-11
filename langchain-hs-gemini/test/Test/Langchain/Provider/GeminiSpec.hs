{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Test.Langchain.Provider.GeminiSpec (tests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (status200, status500)
import Network.Wai (Application, responseLBS, responseStream)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Builder as Builder
import Gemini.Client (InteractionsInput (..))
import Langchain.Cache.Core (CacheableChatModel (..))
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Embeddings.Core (Embeddings (..))
import Langchain.Embeddings.Gemini (GeminiEmbeddings (..), defaultGeminiEmbeddings)
import Langchain.Model
import Langchain.Provider.Gemini
import Langchain.Stream (StreamEvent (..), collectEvents)
import Langchain.Tool (Tool (..), createTool)
import Langchain.Tool.Binding (ToolBinder (..))

withMockServer :: Application -> (T.Text -> IO a) -> IO a
withMockServer app action =
  testWithApplication (pure app) $ \port ->
    action $ "http://127.0.0.1:" <> T.pack (show port)

getLiveApiKey :: IO (Maybe T.Text)
getLiveApiKey = do
  mbEnv <- lookupEnv "GEMINI_API_KEY"
  case mbEnv of
    Just k | not (null k) -> pure (Just (T.pack k))
    _ -> do
      home <- getHomeDirectory
      let p = home </> ".gemini_api_key"
      exists <- doesFileExist p
      if exists
        then do
          txt <- T.strip <$> TIO.readFile p
          pure $ if T.null txt then Nothing else Just txt
        else pure Nothing

tests :: TestTree
tests =
  testGroup
    "Langchain.Provider.GeminiSpec"
    [ testGroup
        "Provider Initialization and Config"
        [ testCase "newGemini initializes correctly" $ do
            let p = newGemini "my-api-key" "gemini-3.6-flash" (Just "http://custom")
            apiKey p @?= "my-api-key"
            model p @?= "gemini-3.6-flash"
            baseUrl p @?= Just "http://custom"
        , testCase "defaultGeminiConfig has empty fields" $ do
            let cfg = defaultGeminiConfig
            configTools cfg @?= Nothing
            configSystemInstruction cfg @?= Nothing
        , testCase "cacheModelIdentity returns correct identity" $ do
            let p = newGemini "key" "gemini-3.6-flash" Nothing
                identity = cacheModelIdentity p (Just defaultGeminiConfig)
            identity @?= [aesonQQ|{"provider": "gemini", "model": "gemini-3.6-flash"}|]
        ]
    , testGroup
        "Message to InteractionsInput mapping"
        [ testCase "single user message maps to TextInput" $ do
            let msgs = [userMessage "Hello world"]
                (mbSys, input) = messagesToInteractionsInput msgs
            mbSys @?= Nothing
            case input of
              TextInput t -> t @?= "Hello world"
              other -> assertFailure ("Expected TextInput, got: " ++ show other)
        , testCase "system and user messages extract system instruction" $ do
            let msgs = [systemMessage "Be brief", userMessage "Hello"]
                (mbSys, _input) = messagesToInteractionsInput msgs
            mbSys @?= Just "Be brief"
        ]
    , testGroup
        "ChatModel invoke (Mock)"
        [ testCase "invoke returns assistant message" $ do
            let app _req respond = do
                  let resp =
                        [aesonQQ|{
                          "id": "v1_test_invoke",
                          "status": "completed",
                          "steps": [
                            {
                              "type": "model_output",
                              "content": [{"type": "text", "text": "Hello from Gemini"}]
                            }
                          ]
                        }|]
                  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)
            withMockServer app $ \url -> do
              let provider = newGemini "fake-key" "gemini-3.6-flash" (Just url)
              eRes <- runExceptT $ invoke provider [userMessage "Hi"] Nothing
              case eRes of
                Left err -> assertFailure (show err)
                Right msg -> do
                  messageRole msg @?= Assistant
                  extractMessageText msg @?= "Hello from Gemini"
        , testCase "invoke redacts API key on error" $ do
            let app _req respond = respond $ responseLBS status500 [] "Internal Server Error"
            withMockServer app $ \url -> do
              let provider = newGemini "SECRET_KEY_12345" "gemini-3.6-flash" (Just url)
              eRes <- runExceptT $ invoke provider [userMessage "Hi"] Nothing
              case eRes of
                Left err -> do
                  let errStr = T.pack (show err)
                  assertBool "Does not contain raw secret key" (not ("SECRET_KEY_12345" `T.isInfixOf` errStr))
                Right _ -> assertFailure "Expected failure on HTTP 500"
        ]
    , testGroup
        "ToolBinder integration"
        [ testCase "bindToolsConfig binds function tool to GeminiConfig" $ do
            let dummyTool :: Tool IO
                dummyTool =
                  createTool
                    "calculator"
                    "Do math"
                    [aesonQQ|{"type": "object"}|]
                    (\_ -> pure (Right "42"))
                cfg :: Maybe GeminiConfig
                cfg = bindToolsConfig @Gemini [dummyTool] Nothing
            case cfg of
              Nothing -> assertFailure "Expected Just GeminiConfig"
              Just c -> case configTools c of
                Just [_] -> pure ()
                other -> assertFailure ("Expected tool in configTools, got: " ++ show other)
        ]
    , testGroup
        "ChatModel stream (Mock)"
        [ testCase "stream yields LLMStart, LLMChunk, and LLMEnd" $ do
            let sseBody =
                  "data: {\"event_type\": \"step.delta\", \"index\": 0, \"delta\": {\"type\": \"text\", \"text\": \"Hello \"}}\n\n"
                    <> "data: {\"event_type\": \"step.delta\", \"index\": 0, \"delta\": {\"type\": \"text\", \"text\": \"World!\"}}\n\n"
                    <> "data: {\"event_type\": \"interaction.completed\", \"interaction\": {\"id\": \"v1_st\", \"status\": \"completed\", \"steps\": [{\"type\": \"model_output\", \"content\": [{\"type\": \"text\", \"text\": \"Hello World!\"}]}]}}\n\n"
                app _req respond =
                  respond $
                    responseStream status200 [("Content-Type", "text/event-stream")] $ \write flush -> do
                      write (Builder.lazyByteString sseBody)
                      flush
            withMockServer app $ \url -> do
              let provider = newGemini "fake-key" "gemini-3.6-flash" (Just url)
              eEvents <- runResourceT $ runExceptT $ collectEvents $ stream provider [userMessage "Hi"] Nothing
              case eEvents of
                Left err -> assertFailure (show err)
                Right events -> do
                  assertBool "Has at least 3 events" (length events >= 3)
                  case (events, reverse events) of
                    (LLMStart {} : _, LLMEnd {} : _) -> pure ()
                    other -> assertFailure ("Unexpected events: " ++ show other)
        ]
    , testGroup
        "Embeddings GeminiEmbeddings (Mock)"
        [ testCase "embedQuery and embedDocuments return vectors" $ do
            let app req respond = do
                  let resp =
                        [aesonQQ|{
                          "embedding": {"values": [0.1, 0.2, 0.3]},
                          "embeddings": [{"values": [0.1, 0.2]}, {"values": [0.3, 0.4]}]
                        }|]
                  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)
            withMockServer app $ \url -> do
              let emb = GeminiEmbeddings "fake-key" "gemini-embedding-001" (Just url)
              -- Query
              eQ <- runExceptT $ embedQuery emb "query text"
              case eQ of
                Left err -> assertFailure (show err)
                Right vec -> vec @?= [0.1, 0.2, 0.3]
              -- Docs
              let doc1 = Document (TL.pack "doc1") Map.empty
                  doc2 = Document (TL.pack "doc2") Map.empty
              eD <- runExceptT $ embedDocuments emb [doc1, doc2]
              case eD of
                Left err -> assertFailure (show err)
                Right vecs -> length vecs @?= 2
        ]
    , testGroup
        "Live Langchain Integration Tests (~/.gemini_api_key)"
        [ testCase "Live invoke ChatModel" $ do
            mbKey <- getLiveApiKey
            case mbKey of
              Nothing -> pure ()
              Just key -> do
                let provider = newGemini key "gemini-3.6-flash" Nothing
                eRes <- runExceptT $ invoke provider [userMessage "Say 'LANGCHAIN_OK'"] Nothing
                case eRes of
                  Left err -> assertFailure ("Live invoke failed: " ++ show err)
                  Right msg -> do
                    messageRole msg @?= Assistant
                    assertBool "Contains text" (not (T.null (extractMessageText msg)))
        , testCase "Live stream ChatModel" $ do
            mbKey <- getLiveApiKey
            case mbKey of
              Nothing -> pure ()
              Just key -> do
                let provider = newGemini key "gemini-3.6-flash" Nothing
                eEvents <-
                  runResourceT $ runExceptT $ collectEvents $ stream provider [userMessage "Count 1, 2, 3"] Nothing
                case eEvents of
                  Left err -> assertFailure ("Live stream failed: " ++ show err)
                  Right events -> do
                    assertBool "Received streaming events" (not (null events))
        , testCase "Live Embeddings (query & docs)" $ do
            mbKey <- getLiveApiKey
            case mbKey of
              Nothing -> pure ()
              Just key -> do
                let emb = defaultGeminiEmbeddings key
                eQ <- runExceptT $ embedQuery emb "Haskell LangChain"
                case eQ of
                  Left err -> assertFailure ("Live embedQuery failed: " ++ show err)
                  Right vec -> assertBool "Dimension > 0" (not (null vec))
                let docs = [Document "Document one" Map.empty, Document "Document two" Map.empty]
                eD <- runExceptT $ embedDocuments emb docs
                case eD of
                  Left err -> assertFailure ("Live embedDocuments failed: " ++ show err)
                  Right vecs -> length vecs @?= 2
        ]
    ]
