{-# LANGUAGE OverloadedStrings #-}

module App.Ollama.Agent (runApp) where

import qualified Data.List.NonEmpty as HM
import Langchain.Agents.Core
import Langchain.Agents.Ollama
import Langchain.LLM.Core
import Langchain.Memory.Core

runApp :: IO ()
runApp = do
  let ag =
        OllamaAgent
          { agentModelName = "mistral"
          , ollamaAvailableTools = defaultOllamaTools
          , agentSystemPrompt = defaultPrompt defaultOllamaTools
          }
  let mem =
        WindowBufferMemory
          { maxWindowSize = 10
          , windowBufferMessages =
              HM.singleton
                (Message System "You are a AI assistant" defaultMessageData)
          }
  let as =
        AgentState
          { agentMemory = mem
          , agentToolResults = []
          , agentSteps = []
          }
  eRes <-
    runAgent
      ag
      as
      "https://tushar-adhatrao.in give me link of any repository from the github link from provided site"
  case eRes of
    Left err -> putStrLn err
    Right aFinish -> do
      print $ returnValues aFinish
