{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.MCP.McpSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import Data.Conduit (fuseUpstream, runConduit)
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.MCP.Client
import Langchain.Model.Types (Role (..), extractMessageText, messageRole)
import Langchain.Tool (StreamingTool (..))
import Langchain.Tool.Core (Tool (..))

tests :: TestTree
tests =
  testGroup
    "Langchain.MCP.McpSpec"
    [ testCase "newStdioMcpClient initializes transport and server name" $ do
        let client = newStdioMcpClient "test-mcp" "npx" ["-y", "@modelcontextprotocol/server-everything"]
        serverName client @?= "test-mcp"
        clientTransport client @?= StdioTransport "npx" ["-y", "@modelcontextprotocol/server-everything"]
    , testCase "mcpToolToLangchainTool converts remote tool to callable local Tool" $ do
        let client = newStdioMcpClient "test-server" "echo" []
            toolInfo =
              McpToolInfo
                { mcpToolName = "echo_tool"
                , mcpToolDescription = "Echoes inputs"
                , mcpToolInputSchema = Aeson.object []
                }
            langchainTool = mcpToolToLangchainTool client toolInfo
        toolName langchainTool @?= "echo_tool"
        toolDescription langchainTool @?= "Echoes inputs"
        res <- toolExecute langchainTool (Aeson.object [])
        case res of
          Left err -> assertFailure ("Tool execution failed: " ++ show err)
          Right out -> assertBool "Executed stdio tool" ("Executed MCP tool" `T.isInfixOf` out)
    , testCase "mcpToolToStreamingTool yields output chunks and returns final text" $ do
        let client = newStdioMcpClient "test-server" "echo" []
            toolInfo =
              McpToolInfo
                { mcpToolName = "stream_tool"
                , mcpToolDescription = "Streaming MCP tool"
                , mcpToolInputSchema = Aeson.object []
                }
            sTool = mcpToolToStreamingTool client toolInfo
        toolName (streamingToolDef sTool) @?= "stream_tool"
        chunksRef <- newIORef ([] :: [T.Text])
        eRes <-
          runConduit
            ( streamingToolExecute sTool (Aeson.object [])
                `fuseUpstream` CL.mapM_ (\c -> modifyIORef' chunksRef (++ [c]))
            )
        case eRes of
          Left err -> assertFailure ("Streaming tool failed: " ++ show err)
          Right out -> do
            assertBool "Got final text" ("Executed MCP tool" `T.isInfixOf` out)
            chunks <- readIORef chunksRef
            case chunks of
              [chunk] -> assertBool "Yielded chunk" ("Executed MCP tool" `T.isInfixOf` chunk)
              _ -> assertFailure ("Expected 1 chunk, got: " ++ show (length chunks))
    , testCase "listMcpResources returns server resources" $ do
        let client = newStdioMcpClient "test-server" "echo" []
        eRes <- runExceptT $ listMcpResources client
        case eRes of
          Left err -> assertFailure ("listMcpResources failed: " ++ show err)
          Right [r] -> do
            mcpResourceUri r @?= "mock://resource/1"
            mcpResourceName r @?= "mock_resource"
            mcpResourceMimeType r @?= Just "text/plain"
          Right other -> assertFailure ("Expected 1 resource, got: " ++ show (length other))
    , testCase "readMcpResource reads resource content by uri" $ do
        let client = newStdioMcpClient "test-server" "echo" []
        eRes <- runExceptT $ readMcpResource client "mock://resource/1"
        case eRes of
          Left err -> assertFailure ("readMcpResource failed: " ++ show err)
          Right content ->
            assertBool "Got resource content" ("Mock content for resource" `T.isInfixOf` content)
    , testCase "listMcpPrompts returns prompt descriptors and arguments" $ do
        let client = newStdioMcpClient "test-server" "echo" []
        eRes <- runExceptT $ listMcpPrompts client
        case eRes of
          Left err -> assertFailure ("listMcpPrompts failed: " ++ show err)
          Right [p] -> do
            mcpPromptName p @?= "mock_prompt"
            mcpPromptDescription p @?= Just "Mock prompt template"
            case mcpPromptArguments p of
              [arg] -> do
                promptArgName arg @?= "topic"
                promptArgRequired arg @?= Just True
              args -> assertFailure ("Expected 1 argument, got: " ++ show (length args))
          Right other -> assertFailure ("Expected 1 prompt, got: " ++ show (length other))
    , testCase "getMcpPrompt maps prompt template messages to Langchain Messages" $ do
        let client = newStdioMcpClient "test-server" "echo" []
            args = Map.fromList [("topic", "Haskell")]
        eRes <- runExceptT $ getMcpPrompt client "explain" (Just args)
        case eRes of
          Left err -> assertFailure ("getMcpPrompt failed: " ++ show err)
          Right [m] -> do
            messageRole m @?= User
            assertBool "Contains prompt topic" ("Haskell" `T.isInfixOf` extractMessageText m)
          Right other -> assertFailure ("Expected 1 message, got: " ++ show (length other))
    ]
