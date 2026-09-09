{-# LANGUAGE OverloadedStrings #-}

module Ollama.MCP (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  let client_ =
        newStdioMcpClient
          "hackage-doc"
          "docker"
          ["run", "-i", "--rm", "tusharknight8/hackage-doc-mcp:latest"]

  res <- runLangchainTIO $ do
    mcpTools <- listMcpTools client_
    let lcTools = map (mcpToolToLangchainTool client_) mcpTools
    liftIO $
      mapM_
        (\t -> T.putStrLn $ "Tool: " <> mcpToolName t <> " - " <> mcpToolDescription t)
        mcpTools

    o <- newOllama "qwen3.5:2b" defaultConfig
    let msgs = [userMessage "Search Hoogle for the Haskell function 'traverse' using the search tool."]
        req = withTools lcTools (chatRequestFor o msgs)

    resp <- invoke o msgs (Just req)
    case messageToolCalls resp of
      Just (tc : _) -> do
        liftIO $ T.putStrLn $ "\nLLM selected tool: " <> toolCallName tc
        liftIO $ T.putStrLn $ "Arguments: " <> T.pack (show (toolCallArguments tc))

        case find (\t -> toolName t == toolCallName tc) lcTools of
          Just tool -> do
            eOut <- liftIO $ toolExecute tool (toolCallArguments tc)
            let toolResult = case eOut of
                  Left err -> "Error: " <> errorMessage err
                  Right out -> out
                toolMsg = (toolMessage toolResult) {messageName = Just (toolCallName tc)}
                conv = msgs ++ [resp, toolMsg]
                followReq = withTools lcTools (chatRequestFor o conv)

            finalResp <- invoke o conv (Just followReq)
            liftIO $ T.putStrLn "\nAI:"
            liftIO $ T.putStrLn $ extractMessageText finalResp
          Nothing ->
            liftIO $ T.putStrLn "Tool not found."
      _ -> do
        liftIO $ T.putStrLn "No tool called by LLM."
        liftIO $ T.putStrLn $ extractMessageText resp

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()
