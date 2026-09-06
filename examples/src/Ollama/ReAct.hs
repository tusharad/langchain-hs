{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ollama.ReAct (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Maybe
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Tool.Calculator (calculatorTool)
import Langchain.Tool.FileSystem (readFileTool)

data ToolModel = ToolModel Ollama [Tool (ExceptT LangchainError IO)]

instance ChatModel ToolModel where
  type ModelConfig ToolModel = ModelConfig Ollama
  invoke (ToolModel model_ tools_) msgs mbReq =
    let req = fromMaybe (chatRequestFor model_ msgs) mbReq
     in invoke model_ msgs (Just (withTools tools_ req))
  stream (ToolModel model_ tools_) msgs mbReq =
    let req = fromMaybe (chatRequestFor model_ msgs) mbReq
     in stream model_ msgs (Just (withTools tools_ req))

runApp :: IO ()
runApp = do
  writeFile "/tmp/budget.txt" "120 + 85"
  o <- newOllama "qwen3.5:2b" defaultConfig
  let tools = [readFileTool, calculatorTool]
      agent = createReActAgent (ToolModel o tools) tools
      prompt =
        [ userMessage
            "Read the expression inside /tmp/budget.txt using read_file, then evaluate it using calculator."
        ]

  res <- runExceptT $ runReActAgent agent prompt
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
