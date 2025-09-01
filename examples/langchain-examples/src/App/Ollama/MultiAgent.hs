{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.Ollama.MultiAgent
  ( runApp
  ) where

import qualified Data.Map.Strict as Map
import Data.Ollama.Common.Utils (encodeImage)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Langchain.Agents.Core
import Langchain.LLM.Ollama
import Langchain.Memory.Core
import Langchain.Tool.WikipediaTool (defaultWikipediaTool)

data (LLM llm) => SimpleToolAgent llm = SimpleToolAgent
  { agentLLM :: llm
  , agentToolList :: [AnyTool]
  }

extractAfter :: Text -> Text -> Text
extractAfter marker text =
  let afterMarker = snd $ T.breakOn marker text
   in if T.null afterMarker
        then ""
        else T.strip $ T.drop 2 $ T.dropWhile (/= ':') afterMarker

parseAgentOutput :: Text -> Either String AgentStep
parseAgentOutput text
  | "FINAL ANSWER:" `T.isInfixOf` text =
      let answer = extractAfter "FINAL ANSWER:" text
       in Right $
            Finish $
              AgentFinish
                { returnValues = Map.singleton "output" answer
                , finishLog = text
                }
  | "TOOL:" `T.isInfixOf` text =
      case T.breakOn "(" (extractAfter "TOOL:" text) of
        (_, rest)
          | not (T.null rest) ->
              let inputWithParen = T.drop 1 rest
                  input = T.dropEnd 1 inputWithParen
               in Right $
                    Continue $
                      AgentAction
                        { actionToolName = T.strip "Wikipedia"
                        , actionInput = T.strip input
                        , actionLog = text
                        }
        _ -> Left $ "Could not parse tool call from text: " <> T.unpack text
  | otherwise =
      Left $
        "Could not parse agent output. Expected 'TOOL:' or 'FINAL ANSWER:'. Got: "
          <> T.unpack text

instance (LLM llm) => Agent (SimpleToolAgent llm) where
  planNextAction SimpleToolAgent {..} AgentState {..} = do
    eMsgs <- messages agentMemory
    case eMsgs of
      Left err -> pure $ Left err
      Right msgs -> do
        eResponse <- chat agentLLM msgs Nothing
        case eResponse of
          Left err -> pure $ Left err
          Right response -> pure $ parseAgentOutput (content response)

  agentTools SimpleToolAgent {..} = pure agentToolList
  agentPrompt _ = pure $ error "This agent does not use a static prompt template."

runApp :: IO ()
runApp = do
  mbEncodedImage <- encodeImage "/home/user/Downloads/pomerian_dog.jpg"
  case mbEncodedImage of
    Nothing -> putStrLn "Failed to load image"
    Just encodedImage -> do
      TIO.putStrLn "Step 1: Using Vision Model to describe the scene..."
      let visionLLM = Ollama "gemma3" []
          visionPrompt = "Describe the main subject of the given photo."
      eDescription <-
        generate
          visionLLM
          visionPrompt
          ( Just
              defaultOllamaParams
                { images = Just [encodedImage]
                }
          )

      case eDescription of
        Left err -> TIO.putStrLn $ "Vision model failed: " <> T.pack err
        Right description -> do
          TIO.putStrLn $ ">> Scene Description: " <> description
          TIO.putStrLn "--------------------------------------------"
          let reasoningLLM = Ollama "qwen3:0.6b" []
              wikiTool = customAnyTool defaultWikipediaTool id id
              infoAgent = SimpleToolAgent reasoningLLM [wikiTool]
          let agentInitialPrompt =
                T.unlines
                  [ "/no_think You are an assistant that provides more details about a subject."
                  , "Based on the following description of an image, "
                      <> "find out more about the main subject using your tools."
                  , "Description: \"" <> description <> "\""
                  , "To call a tool, Only respond with TOOL: (<single word query>)"
                  , "The single word query should be the subject you want info of"
                  , "For e.g to get info about pitbull dog, respond with TOOL: (pitbull_dog)"
                  , "Once you have enough information, write final answer with FINAL ANSWER:"
                  , "Only respond with either TOOL: or FINAL ANSWER: and nothing else"
                  , "Your goal is to provide details about the subject "
                      <> " of the image not just image description"
                  ]

          let initialState =
                AgentState
                  { agentMemory =
                      WindowBufferMemory
                        5
                        (initialChatMessage "You are a helpful assistant.")
                  , agentToolResults = []
                  , agentSteps = []
                  }

          finalResult <- runAgent infoAgent initialState agentInitialPrompt
          case finalResult of
            Left agentErr -> TIO.putStrLn $ "\nAgent failed: " <> T.pack agentErr
            Right agentFinish -> do
              let finalAnswer =
                    Map.findWithDefault
                      "No answer found."
                      "output"
                      (returnValues agentFinish)
              TIO.putStrLn "\n>> Agent Finished. Final Answer:"
              TIO.putStrLn finalAnswer
