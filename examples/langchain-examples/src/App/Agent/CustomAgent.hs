{- |
Module      : App.Agent.CustomAgent
Description : Example showing how to create a custom agent
Copyright   : (c) 2025 Tushar Adhatrao
License     : MIT

This example demonstrates how to create a custom agent by implementing
the Agent typeclass directly, giving you full control over the planning
and execution logic.

Usage:
> cabal run langchain-examples -- custom-agent
-}
module App.Agent.CustomAgent () where

{-
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Agent.Core
import Langchain.Agent.Executor
import Langchain.Agent.Tools
import Langchain.Error (LangchainError, LangchainResult, agentError)
import Langchain.LLM.Core (ChatHistory, LLM, Message (..), Role (..), chat, defaultMessageData)

{- | Custom agent that uses a simple rule-based approach.

Instead of using ReAct, this agent:
1. Checks if the input contains keywords
2. Routes to appropriate tools based on keywords
3. Returns results directly
-}
data SimpleRuleAgent llm = SimpleRuleAgent
  { ruleAgentLLM :: llm
  , ruleAgentTools :: ToolRegistry
  }

instance LLM llm => Agent (SimpleRuleAgent llm) where
  plan agent state = do
    let input = T.toLower (agentInput state)
        iterations = agentIterations state

    -- Check iteration limit
    if iterations >= 5
      then
        return $
          Right $
            Right $
              AgentFinish
                { agentOutput = "Maximum iterations reached"
                , finishMetadata = Map.empty
                , finishLog = "Stopped due to iteration limit"
                }
      else
        -- Simple rule-based routing
        if "calculate" `T.isInfixOf` input || "+" `T.isInfixOf` input || "*" `T.isInfixOf` input
          then do
            -- Extract the expression (simplified)
            let expression = extractExpression input
            return $
              Right $
                Left $
                  AgentAction
                    { actionTool = "calculator"
                    , actionToolInput = expression
                    , actionLog = "Detected calculation request"
                    , actionMetadata = Map.empty
                    }
          else
            if "search" `T.isInfixOf` input || "find" `T.isInfixOf` input
              then
                return $
                  Right $
                    Left $
                      AgentAction
                        { actionTool = "search"
                        , actionToolInput = input
                        , actionLog = "Detected search request"
                        , actionMetadata = Map.empty
                        }
              else
                -- No specific action, use LLM to decide
                planWithLLM agent state

  getTools agent = []

  executeTool agent toolName input = do
    case lookupTool toolName (ruleAgentTools agent) of
      Nothing ->
        return $ Left $ agentError ("Tool not found: " <> toolName) Nothing Nothing
      Just executor -> executor input

{- | Extract mathematical expression from text.

This is a simplified version - in production you'd want better parsing.
-}
extractExpression :: Text -> Text
extractExpression text =
  let words = T.words text
      expr = filter isExprWord words
   in T.unwords expr
  where
    isExprWord w =
      T.any (`elem` ("0123456789+-*/()" :: String)) w

-- | Use LLM as fallback when rules don't match.
planWithLLM ::
  LLM llm =>
  SimpleRuleAgent llm -> AgentState -> IO (LangchainResult (Either AgentAction AgentFinish))
planWithLLM agent state = do
  let prompt =
        "Given this input: "
          <> agentInput state
          <> "\n\nProvide a brief, direct answer."
      messages =
        NE.fromList
          [ Message
              { role = User
              , content = prompt
              , messageData = defaultMessageData
              }
          ]

  eResponse <- chat (ruleAgentLLM agent) messages Nothing
  case eResponse of
    Left err -> return $ Left err
    Right responseMsg ->
      return $
        Right $
          Right $
            AgentFinish
              { agentOutput = content responseMsg
              , finishMetadata = Map.empty
              , finishLog = "LLM provided direct answer"
              }

-- | Run the custom agent example.
runApp :: IO ()
runApp = do
  putStrLn "=== Custom Rule-Based Agent Example ==="
  putStrLn ""

  -- This would need actual tool implementations
  -- For now, we'll show the structure

  putStrLn "This example demonstrates the structure of a custom agent."
  putStrLn "In a real implementation, you would:"
  putStrLn "  1. Implement your custom planning logic"
  putStrLn "  2. Add your tools to the registry"
  putStrLn "  3. Handle tool execution"
  putStrLn "  4. Define termination conditions"
  putStrLn ""
  putStrLn "The key advantage of custom agents is complete control"
  putStrLn "over the decision-making process, which can be more"
  putStrLn "efficient for specific use cases."
  -}
