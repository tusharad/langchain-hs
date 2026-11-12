{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Langchain.Tool (tests) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Conduit (fuseUpstream, runConduit, yield)
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Error
import Langchain.Stream (StreamEvent (ToolChunk, ToolEnd, ToolErrorEvent, ToolStart))
import Langchain.Tool
import Langchain.Tool.Security (readOnlySecurityPolicy, unrestrictedSecurityPolicy)
import Langchain.Tool.Shell (streamingShellTool)
import Test.Tasty
import Test.Tasty.HUnit

calcExec :: Value -> IO (Either LangchainError Text)
calcExec (Object _) = pure $ Right "42"
calcExec _ = pure $ Left $ toolError "Invalid arguments" (Just "calc") Nothing

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool"
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
    , testCase "streamToolCall emits ToolStart and ToolEnd on success" $ do
        let t = createTool "calculator" "Performs math" (object []) calcExec
        eventsRef <- newIORef ([] :: [StreamEvent])
        res <-
          runConduit $
            streamToolCall "run-1" t (object [])
              `fuseUpstream` CL.mapM_ (\ev -> modifyIORef' eventsRef (++ [ev]))
        res @?= Right "42"
        events <- readIORef eventsRef
        events
          @?= [ ToolStart "run-1" "calculator" (object [])
              , ToolEnd "run-1" "calculator" (toJSON ("42" :: Text))
              ]
    , testCase "streamToolCall emits ToolStart and ToolErrorEvent on failure" $ do
        let t = createTool "calculator" "Performs math" (object []) calcExec
        eventsRef <- newIORef ([] :: [StreamEvent])
        res <-
          runConduit $
            streamToolCall "run-2" t (Number 1)
              `fuseUpstream` CL.mapM_ (\ev -> modifyIORef' eventsRef (++ [ev]))
        case res of
          Left _ -> pure ()
          Right out -> assertFailure $ "Expected error, got: " ++ show out
        events <- readIORef eventsRef
        case events of
          [ToolStart "run-2" "calculator" (Number 1), ToolErrorEvent "run-2" "calculator" _] -> pure ()
          _ -> assertFailure $ "Unexpected events: " ++ show events
    , testCase "streamStreamingToolCall emits incremental ToolChunk events" $ do
        let sTool =
              createStreamingTool "streamer" "Streams chunks" (object []) $ \_ -> do
                yield "chunk-1 "
                yield "chunk-2"
                pure (Right "chunk-1 chunk-2")
        eventsRef <- newIORef ([] :: [StreamEvent])
        res <-
          runConduit $
            streamStreamingToolCall "run-3" sTool (object [])
              `fuseUpstream` CL.mapM_ (\ev -> modifyIORef' eventsRef (++ [ev]))
        res @?= Right "chunk-1 chunk-2"
        events <- readIORef eventsRef
        events
          @?= [ ToolStart "run-3" "streamer" (object [])
              , ToolChunk "run-3" "streamer" "chunk-1 "
              , ToolChunk "run-3" "streamer" "chunk-2"
              , ToolEnd "run-3" "streamer" (toJSON ("chunk-1 chunk-2" :: Text))
              ]
    , testCase "streamingShellTool streams stdout lines and enforces SecurityPolicy" $ do
        let sShell = streamingShellTool unrestrictedSecurityPolicy
        eventsRef <- newIORef ([] :: [StreamEvent])
        res <-
          runConduit $
            streamStreamingToolCall
              "run-sh"
              sShell
              (object ["command" .= ("printf 'line1\\nline2\\n'" :: Text)])
              `fuseUpstream` CL.mapM_ (\ev -> modifyIORef' eventsRef (++ [ev]))
        res @?= Right "line1\nline2"
        events <- readIORef eventsRef
        let chunks = [txt | ToolChunk _ _ txt <- events]
        chunks @?= ["line1\n", "line2\n"]

        let roShell = streamingShellTool readOnlySecurityPolicy
        roRes <- toolExecute (streamingToolDef roShell) (object ["command" .= ("echo hi" :: Text)])
        case roRes of
          Left err -> assertBool "Mentions permission" ("permission" `T.isInfixOf` T.toLower (errorMessage err))
          Right ok -> assertFailure $ "Expected security denial, got: " ++ show ok
    ]
