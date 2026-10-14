{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-incomplete-patterns #-}

{- |
Module      : Test.Gemini.Client.InteractionsSpec
Description : Test every single example from https://ai.google.dev/api/interactions-api-v1
-}
module Test.Gemini.Client.InteractionsSpec (tests) where

import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, rawPathInfo, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Gemini.Client

-- | Helper to start a local mock Warp server on a free port
withMockServer :: Application -> (Text -> IO a) -> IO a
withMockServer app action =
  testWithApplication (pure app) $ \port ->
    action ("http://127.0.0.1:" <> T.pack (show port))

-- | Read GEMINI API KEY from env or ~/.gemini_api_key
getLiveApiKey :: IO (Maybe Text)
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
    "Gemini.Client.InteractionsSpec (All Documentation Examples)"
    [ mockExamplesTests
    , liveExamplesTests
    ]

-- ---------------------------------------------------------------------------
-- 1. Mock Server Tests (Verifies all 14 examples against exact API contracts)
-- ---------------------------------------------------------------------------

mockExamplesTests :: TestTree
mockExamplesTests =
  testGroup
    "Mock API Contract Tests"
    [ testCase "Example 1: Simple Request" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_simple",
                      "object": "interaction",
                      "status": "completed",
                      "model": "gemini-3.6-flash",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Why did the chicken cross the road? To get to the other side!"}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req = createModelInteraction "gemini-3.6-flash" (TextInput "Tell me a joke")
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> do
              interactionId inter @?= "v1_mock_simple"
              interactionStatus inter @?= StatusCompleted
              case interactionSteps inter of
                [ModelOutputStep [TextContent txt]] ->
                  assertBool "contains joke" ("chicken" `T.isInfixOf` txt)
                other -> assertFailure ("Unexpected steps: " ++ show other)
    , testCase "Example 2: Multi-turn Conversation" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_multiturn",
                      "object": "interaction",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Your name is Bob."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let steps =
                [ UserInputStep [TextContent "Hi, I am Bob"]
                , ModelOutputStep [TextContent "Hello Bob! Nice to meet you."]
                , UserInputStep [TextContent "What is my name?"]
                ]
              req = createModelInteraction "gemini-3.6-flash" (StepListInput steps)
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionStatus inter @?= StatusCompleted
    , testCase "Example 3: Multimodal - Image Input" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_image",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "This is a red circle."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let contents =
                [ TextContent "What is in this image?"
                , ImageContent
                    "image/png"
                    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII="
                ]
              req = createModelInteraction "gemini-3.6-flash" (ContentListInput contents)
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionStatus inter @?= StatusCompleted
    , testCase "Example 4: Function Calling (requires_action -> function_result)" $ do
        let app req respond = do
              body <- strictRequestBody req
              let isFollowUp = "\"function_result\"" `BS.isInfixOf` LBS.toStrict body
                  resp =
                    if isFollowUp
                      then
                        [aesonQQ|{
                          "id": "v1_mock_fc_step2",
                          "status": "completed",
                          "steps": [
                            {
                              "type": "model_output",
                              "content": [{"type": "text", "text": "The weather in Tokyo is 20°C and sunny."}]
                            }
                          ]
                        }|]
                      else
                        [aesonQQ|{
                          "id": "v1_mock_fc_step1",
                          "status": "requires_action",
                          "steps": [
                            {
                              "type": "function_call",
                              "id": "call_tokyo",
                              "name": "get_weather",
                              "arguments": {"location": "Tokyo"}
                            }
                          ]
                        }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let tool = functionTool "get_weather" (Just "Get current weather") (Just [aesonQQ|{"type": "object"}|])
              req1 =
                (createModelInteraction "gemini-3.6-flash" (TextInput "What is the weather in Tokyo?"))
                  { reqTools = Just [tool]
                  }
          res1 <- runGeminiClient env (createInteraction "fake-key" req1)
          case res1 of
            Left err -> assertFailure (show err)
            Right inter1 -> do
              interactionStatus inter1 @?= StatusRequiresAction
              case interactionSteps inter1 of
                [FunctionCallStep {callId = cid, callName = cname}] -> do
                  cid @?= "call_tokyo"
                  cname @?= "get_weather"
                  -- Follow up with function_result
                  let resultStep = FunctionResultStep cid (Just cname) [aesonQQ|{"temperature": 20, "condition": "sunny"}|] Nothing
                      req2 =
                        (defaultCreateInteractionRequest (StepListInput [resultStep]))
                          { reqModel = Just "gemini-3.6-flash"
                          , reqPreviousInteractionId = Just (interactionId inter1)
                          }
                  res2 <- runGeminiClient env (createInteraction "fake-key" req2)
                  case res2 of
                    Left err2 -> assertFailure (show err2)
                    Right inter2 -> interactionStatus inter2 @?= StatusCompleted
                other -> assertFailure ("Expected FunctionCallStep, got: " ++ show other)
    , testCase "Example 5: Deep Research Agent" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_deep_research",
                      "agent": "deep-research",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Deep research findings on quantum computing."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req = createAgentInteraction "deep-research" (TextInput "Research quantum computing breakthroughs")
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> do
              interactionAgent inter @?= Just "deep-research"
              interactionStatus inter @?= StatusCompleted
    , testCase "Example 6: Antigravity Agent" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_antigravity",
                      "agent": "antigravity",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Antigravity task completed."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req = createAgentInteraction "antigravity" (TextInput "Solve the algorithmic puzzle")
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> do
              interactionAgent inter @?= Just "antigravity"
              interactionStatus inter @?= StatusCompleted
    , testCase "Example 7: Reuse Env (previous_interaction_id)" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_reuse",
                      "previous_interaction_id": "v1_mock_prev",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Continuing previous session."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req =
                (createModelInteraction "gemini-3.6-flash" (TextInput "Continue our conversation"))
                  { reqPreviousInteractionId = Just "v1_mock_prev"
                  }
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionPreviousInteractionId inter @?= Just "v1_mock_prev"
    , testCase "Example 8: With Sources / Grounding" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_sources",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "google_search_result",
                          "call_id": "call_search_1",
                          "result": [
                            {
                              "title": "Example Source",
                              "url": "https://example.com/source",
                              "snippet": "Information from source"
                            }
                          ]
                        },
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "According to Example Source, ..."}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req =
                (createModelInteraction "gemini-3.6-flash" (TextInput "Find recent news"))
                  { reqTools = Just [GoogleSearchTool]
                  }
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> do
              let searchSteps = [s | s@(GoogleSearchResultStep {}) <- interactionSteps inter]
              assertBool "Contains google_search_result step" (not (null searchSteps))
    , testCase "Example 9: Custom Agent / System Instruction" $ do
        let app req respond = do
              body <- strictRequestBody req
              let expectedResp =
                    [aesonQQ|{
                      "id": "v1_mock_pirate",
                      "status": "completed",
                      "system_instruction": "You are a pirate.",
                      "steps": [
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "Ahoy matey!"}]
                        }
                      ]
                    }|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          let req =
                (createModelInteraction "gemini-3.6-flash" (TextInput "Hello"))
                  { reqSystemInstruction = Just "You are a pirate."
                  }
          res <- runGeminiClient env (createInteraction "fake-key" req)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionSystemInstruction inter @?= Just "You are a pirate."
    , testCase
        "Example 10: Built-in Tools (CodeExecution, GoogleSearch, GoogleMaps, FileSearch, UrlContext)"
        $ do
          let app req respond = do
                body <- strictRequestBody req
                let expectedResp =
                      [aesonQQ|{
                      "id": "v1_mock_tools",
                      "status": "completed",
                      "steps": [
                        {
                          "type": "code_execution_call",
                          "id": "call_py",
                          "arguments": {"code": "print(2+2)"}
                        },
                        {
                          "type": "code_execution_result",
                          "call_id": "call_py",
                          "result": "4\n"
                        },
                        {
                          "type": "model_output",
                          "content": [{"type": "text", "text": "2 + 2 = 4"}]
                        }
                      ]
                    }|]
                respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
          withMockServer app $ \base -> do
            env <- newGeminiClientEnvWithBaseUrl base "fake-key"
            let tools =
                  [ CodeExecutionTool
                  , GoogleSearchTool
                  , GoogleMapsTool (Just 37.77) (Just (-122.41))
                  , FileSearchTool (Just ["my-store"])
                  , UrlContextTool
                  ]
                req =
                  (createModelInteraction "gemini-3.6-flash" (TextInput "Run math code"))
                    { reqTools = Just tools
                    }
            res <- runGeminiClient env (createInteraction "fake-key" req)
            case res of
              Left err -> assertFailure (show err)
              Right inter -> do
                let pyCalls = [s | s@(CodeExecutionCallStep {}) <- interactionSteps inter]
                assertBool "Contains CodeExecutionCallStep" (not (null pyCalls))
    , testCase "Example 11: Cancel Interaction" $ do
        let app req respond = do
              let p = rawPathInfo req
                  m = requestMethod req
              if m == "POST" && "/cancel" `BS.isSuffixOf` p
                then do
                  let expectedResp = [aesonQQ|{"id": "v1_cancel_me", "status": "cancelled", "steps": []}|]
                  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
                else respond $ responseLBS status404 [] "not found"
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          res <- runGeminiClient env (cancelInteraction "fake-key" "v1_cancel_me")
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionStatus inter @?= StatusCancelled
    , testCase "Example 12: Get Interaction" $ do
        let app req respond = do
              let expectedResp = [aesonQQ|{"id": "v1_got_it", "status": "completed", "steps": []}|]
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode expectedResp)
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          res <- runGeminiClient env (getInteraction "fake-key" "v1_got_it" Nothing Nothing)
          case res of
            Left err -> assertFailure (show err)
            Right inter -> interactionId inter @?= "v1_got_it"
    , testCase "Example 13: Delete Interaction" $ do
        let app req respond = do
              respond $ responseLBS status200 [("Content-Type", "application/json")] "{}"
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          res <- runGeminiClient env (deleteInteraction "fake-key" "v1_del_me")
          case res of
            Left err -> assertFailure (show err)
            Right val -> val @?= [aesonQQ|{}|]
    , testCase "Example 14: Embeddings (embedContent and batchEmbedContents)" $ do
        let app req respond = do
              let p = rawPathInfo req
              if ":embedContent" `BS.isSuffixOf` p
                then do
                  let resp = [aesonQQ|{"embedding": {"values": [0.1, 0.2, 0.3]}}|]
                  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)
                else
                  if ":batchEmbedContents" `BS.isSuffixOf` p
                    then do
                      let resp = [aesonQQ|{"embeddings": [{"values": [0.1, 0.2]}, {"values": [0.3, 0.4]}]}|]
                      respond $ responseLBS status200 [("Content-Type", "application/json")] (encode resp)
                    else respond $ responseLBS status404 [] "not found"
        withMockServer app $ \base -> do
          env <- newGeminiClientEnvWithBaseUrl base "fake-key"
          -- Single
          let singleReq = EmbedContentRequest (EmbedContentPayload [EmbedPart "Query text"])
          res1 <- runGeminiClient env (embedContent "fake-key" "gemini-embedding-001" singleReq)
          case res1 of
            Left err -> assertFailure (show err)
            Right r -> embeddingValues (responseEmbedding r) @?= [0.1, 0.2, 0.3]
          -- Batch
          let batchReq =
                BatchEmbedContentsRequest
                  [ BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "Doc 1"])
                  , BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "Doc 2"])
                  ]
          res2 <- runGeminiClient env (batchEmbedContents "fake-key" "gemini-embedding-001" batchReq)
          case res2 of
            Left err -> assertFailure (show err)
            Right r -> length (batchEmbeddings r) @?= 2
    ]

-- ---------------------------------------------------------------------------
-- 2. Live API Tests (executed when ~/.gemini_api_key or GEMINI_API_KEY is present)
-- ---------------------------------------------------------------------------

liveExamplesTests :: TestTree
liveExamplesTests =
  testGroup
    "Live Google Gemini API Tests (~/.gemini_api_key)"
    [ testCase "Live: Simple Request, Get, and Delete Interaction lifecycle" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure () -- Skip if no live key available
          Just key -> do
            env <- newGeminiClientEnv key
            let req = createModelInteraction "gemini-3.6-flash" (TextInput "Reply with the exact word PONG")
            resCreate <- runGeminiClient env (createInteraction key req)
            case resCreate of
              Left err -> assertFailure ("Live createInteraction failed: " ++ show err)
              Right inter -> do
                let interId = interactionId inter
                assertBool "Interaction ID is non-empty" (not (T.null interId))
                -- Test GET interaction
                resGet <- runGeminiClient env (getInteraction key interId Nothing Nothing)
                case resGet of
                  Left err -> assertFailure ("Live getInteraction failed: " ++ show err)
                  Right gotInter -> interactionId gotInter @?= interId
                -- Test DELETE interaction
                resDel <- runGeminiClient env (deleteInteraction key interId)
                case resDel of
                  Left err -> assertFailure ("Live deleteInteraction failed: " ++ show err)
                  Right _ -> pure ()
    , testCase "Live: Multi-turn Conversation" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            let steps =
                  [ UserInputStep [TextContent "My favorite fruit is Mango."]
                  , ModelOutputStep [TextContent "Got it! Your favorite fruit is Mango."]
                  , UserInputStep [TextContent "What is my favorite fruit? Reply in one word."]
                  ]
                req = createModelInteraction "gemini-3.6-flash" (StepListInput steps)
            res <- runGeminiClient env (createInteraction key req)
            case res of
              Left err -> assertFailure ("Live multi-turn failed: " ++ show err)
              Right inter -> do
                interactionStatus inter @?= StatusCompleted
                -- Clean up
                _ <- runGeminiClient env (deleteInteraction key (interactionId inter))
                pure ()
    , testCase "Live: Function Calling (Tool use)" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            let weatherTool =
                  functionTool
                    "get_weather"
                    (Just "Get the weather for a location")
                    ( Just
                        [aesonQQ|{
                      "type": "object",
                      "properties": {
                        "location": {"type": "string"}
                      },
                      "required": ["location"]
                    }|]
                    )
                req =
                  (createModelInteraction "gemini-3.6-flash" (TextInput "What is the weather in Tokyo?"))
                    { reqTools = Just [weatherTool]
                    }
            res <- runGeminiClient env (createInteraction key req)
            case res of
              Left err -> assertFailure ("Live tool call failed: " ++ show err)
              Right inter -> do
                interactionStatus inter @?= StatusRequiresAction
                case [s | s@(FunctionCallStep {}) <- interactionSteps inter] of
                  (FunctionCallStep {..} : _) -> do
                    callName @?= "get_weather"
                    -- Follow up with tool result
                    let resultStep =
                          FunctionResultStep
                            callId
                            (Just callName)
                            [aesonQQ|{"temperature": 22, "condition": "Sunny"}|]
                            Nothing
                        followUp =
                          (defaultCreateInteractionRequest (StepListInput [resultStep]))
                            { reqModel = Just "gemini-3.6-flash"
                            , reqPreviousInteractionId = Just (interactionId inter)
                            }
                    resFollowUp <- runGeminiClient env (createInteraction key followUp)
                    case resFollowUp of
                      Left fErr -> assertFailure ("Live follow-up failed: " ++ show fErr)
                      Right completedInter -> do
                        interactionStatus completedInter @?= StatusCompleted
                        -- Clean up
                        _ <- runGeminiClient env (deleteInteraction key (interactionId completedInter))
                        pure ()
                  [] -> assertFailure "Expected FunctionCallStep in interaction steps"
                _ <- runGeminiClient env (deleteInteraction key (interactionId inter))
                pure ()
    , testCase "Live: Built-in Code Execution Tool" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            let req =
                  (createModelInteraction "gemini-3.6-flash" (TextInput "Run python code to compute 999 * 333"))
                    { reqTools = Just [CodeExecutionTool]
                    }
            res <- runGeminiClient env (createInteraction key req)
            case res of
              Left err -> assertFailure ("Live code execution failed: " ++ show err)
              Right inter -> do
                interactionStatus inter @?= StatusCompleted
                _ <- runGeminiClient env (deleteInteraction key (interactionId inter))
                pure ()
    , testCase "Live: Built-in Google Search Tool" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            let req =
                  (createModelInteraction "gemini-3.6-flash" (TextInput "Who won the cricket world cup in 2023?"))
                    { reqTools = Just [GoogleSearchTool]
                    }
            res <- runGeminiClient env (createInteraction key req)
            case res of
              Left err -> assertFailure ("Live google search failed: " ++ show err)
              Right inter -> do
                interactionStatus inter @?= StatusCompleted
                _ <- runGeminiClient env (deleteInteraction key (interactionId inter))
                pure ()
    , testCase "Live: Background Interaction and Cancel" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            let req =
                  (createModelInteraction "gemini-3.6-flash" (TextInput "Write a detailed history of the universe"))
                    { reqBackground = Just True
                    }
            resCreate <- runGeminiClient env (createInteraction key req)
            case resCreate of
              Left err -> assertFailure ("Live background create failed: " ++ show err)
              Right inter -> do
                let interId = interactionId inter
                resCancel <- runGeminiClient env (cancelInteraction key interId)
                case resCancel of
                  Left err -> assertFailure ("Live cancel failed: " ++ show err)
                  Right cancelled -> do
                    interactionStatus cancelled @?= StatusCancelled
                    _ <- runGeminiClient env (deleteInteraction key interId)
                    pure ()
    , testCase "Live: Embeddings (gemini-embedding-001)" $ do
        mbKey <- getLiveApiKey
        case mbKey of
          Nothing -> pure ()
          Just key -> do
            env <- newGeminiClientEnv key
            -- Single embed
            let singleReq = EmbedContentRequest (EmbedContentPayload [EmbedPart "Haskell LangChain"])
            resSingle <- runGeminiClient env (embedContent key "gemini-embedding-001" singleReq)
            case resSingle of
              Left err -> assertFailure ("Live embedContent failed: " ++ show err)
              Right resp -> do
                let vec = embeddingValues (responseEmbedding resp)
                assertBool "Embedding dimension > 0" (not (null vec))
            -- Batch embed
            let batchReq =
                  BatchEmbedContentsRequest
                    [ BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "Hello"])
                    , BatchEmbedRequestItem "models/gemini-embedding-001" (EmbedContentPayload [EmbedPart "World"])
                    ]
            resBatch <- runGeminiClient env (batchEmbedContents key "gemini-embedding-001" batchReq)
            case resBatch of
              Left err -> assertFailure ("Live batchEmbedContents failed: " ++ show err)
              Right resp -> length (batchEmbeddings resp) @?= 2
    ]
