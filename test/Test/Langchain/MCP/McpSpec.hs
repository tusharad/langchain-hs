{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.MCP.McpSpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.MCP.Client
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
    ]
