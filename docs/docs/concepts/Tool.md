---
sidebar_position: 11
---

# Tool 

`Tool` typeclass provides an interface to define a tool that can be used in Agents. runTool is a function that takes an input and produces an output. The `Tool` typeclass defines methods for executing the tool, as well as for managing the metadata associated with the tool, such as its name and description.

The `Tool` typeclass is designed to be flexible and extensible, allowing developers to implement their own tools with custom input and output types.

## Supported Integrations

At this moment, following integrations available,

- WikipediaTool
- WebScraperTool
- CalculatorTool
- DuckDuckGo

## Example 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ScrapperTool (runApp) where

import Data.List.NonEmpty (fromList)
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import Langchain.Tool.Core
import Langchain.Tool.WebScraper

runApp :: IO ()
runApp = do
    eRes <- runTool WebScraper "https://tushar-adhatrao.in"
    case eRes of
        Left _ -> pure ()
        Right htmlContent -> do
            let o = Ollama "qwen3:4b" []
            e <-
                chat
                    o
                    ( fromList
                        [ Message
                            System
                            ( "Answer questions based on given below scraped html content: "
                                <> htmlContent
                            )
                            defaultMessageData
                        , Message
                            User
                            "Who is Tushar? /no_think"
                            defaultMessageData
                        ]
                    )
                    Nothing
            print e
```

#### Output

```bash
Right "<think>\n\n</think>\n\nTushar is a friendly neighborhood functional programmer. He is known for his skills in areas such as Haskell, PostgreSQL, Docker, GraphQL, Kubernetes, and GCP. He is currently learning or interested in Rust, Web Assembly, and Large Language Models (LLMs). You can find more about him on his LinkedIn profile, GitHub, and contact him via email."
```

:::warning

Ollama's default context is quite [small](https://github.com/ollama/ollama/blob/main/docs/faq.md#how-can-i-specify-the-context-window-size). You can increase the window size by passing num_ctx in option parameter.

:::

## WikipediaTool Example 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module WikipediaQA (runApp, askQuestion) where

import Data.Aeson
import qualified Data.List.NonEmpty as NE
import Langchain.Callback (stdOutCallback)
import Langchain.LLM.Core
import Langchain.LLM.Ollama
import Langchain.Tool.Core (Tool (runTool))
import Data.Text
import Langchain.Tool.WikipediaTool

data Conversation = Conversation
  { llm :: Ollama
  , messages :: NE.NonEmpty Message
  }

initConversation :: Text -> IO Conversation
initConversation topic = do
  let ollamaLLM =
        Ollama
          { modelName = "qwen3:4b"
          , callbacks = [stdOutCallback]
          }
  let wTool =
        defaultWikipediaTool
          { docMaxChars = 5000
          }
  wikiContent <- runTool wTool topic
  let initialMessages =
        NE.fromList
          [ Message
              System
              ( "You are a helpful assistant, answer user's query based on below wikipedia content: "
                  <> wikiContent
              )
              defaultMessageData
          ]
  pure $ Conversation ollamaLLM initialMessages

askQuestion :: Conversation -> Text -> IO (Either String Text, Conversation)
askQuestion conv question = do
  let newMessages = messages conv <> NE.fromList [Message User question defaultMessageData]
  eRes <- chat
    (llm conv)
    newMessages
    ( Just $
        defaultOllamaParams
          { options = Just (object [("num_ctx", Number 10000)])
          }
    )
  case eRes of
    Left err -> pure (Left err, conv { messages = newMessages })
    Right answer -> do
      let updatedMessages = newMessages <> NE.fromList [Message Assistant answer defaultMessageData]
      pure (Right answer, conv { messages = updatedMessages })

runApp :: IO ()
runApp = do
  conv <- initConversation "Superman_(2025_film)"
  (res1, conv1) <- askQuestion conv "When is James Gunn's superman releasing? /no_think"
  case res1 of
    Left _ -> pure ()
    Right r -> print r
  (res2, _) <- askQuestion conv1 "Who is the lead actor in it? /no_think"
  print res2
```

#### Output 

```bash
Model operation started
Model completed with
"<think>\n\n</think>\n\nJames Gunn's Superman film is scheduled to be released theatrically in the United States on **July 11, 2025**."
Model operation started
Model completed with
Right "<think>\n\n</think>\n\nThe lead actor in James Gunn's *Superman* is **David Corenswet**, who plays Clark Kent / Superman."
```

### Calculator 

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.Tool.Core
import Langchain.Tool.Calculator

runApp :: IO ()
runApp = do
   res <- runTool CalculatorTool "2.0+(1 - 2) * 5"
   print res
```

Calculator tool can perform Add, Sub, Mul, Div, Pow operation and return type would be Either String Double.

:::Warning

Right now, duckduckgo tool only returns result if the search term has an abstract card. 
It does not return links, only abstract summary

:::
