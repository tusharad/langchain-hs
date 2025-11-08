---
sidebar_position: 1
---
import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

# Quickstart: Langchain-hs

### Pre-requisites

1. Install GHC and Stack via [GHCup](https://www.haskell.org/ghcup/)
2. For Ollama, Download and install [Ollama](https://ollama.com/download) and make sure the model you want to use is installed. You can check the list of models using `ollama list` command or install a model using `ollama pull <model-name>` command. 

### Steps 

- Add langchain-hs to your project

```yaml title="package.yaml"
dependencies:
- base < 5
- langchain-hs
```

### Example of generating response from a single prompt.

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>
  <TabItem value="ollama">

    ```haskell
    {-# LANGUAGE OverloadedStrings #-}

    module LangchainLib (runApp) where

    import Langchain.LLM.Ollama (Ollama(..))
    import Langchain.LLM.Core
    import qualified Data.Text as T

    runApp :: IO ()
    runApp = do
    let ollamaLLM = Ollama "llama3.2" []  
    genResult <- generate ollamaLLM "Explain Haskell in simple terms." Nothing
    case genResult of
        Left err -> putStrLn $ "Generate error: " ++ err
        Right text -> putStrLn $ "Generated Text:\n" ++ T.unpack text
    ```

In above code:

1. Setup the `Ollama` LLM with the model name and optional list of callback functions.
2. Call the `generate` function with the prompt and optional parameters.
3. Handle the result, which can be either an error or the generated text.
4. The `generate` function returns a `Text` response, which you can print or use as needed.

:::warning

For Ollama, Make sure the model that you want to use is installed on your local machine; else it will throw error.

:::
  </TabItem>

  <TabItem value="openai">

  ```haskell
  {-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI (OpenAI(..))

main :: IO ()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , callbacks = []
        , baseUrl = Nothing
        }
  result <- LLM.generate openAI "Tell me a joke" Nothing
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right response -> putStrLn response
    ```
In above code:
1. Setup the `OpenAI` LLM with the api key and optional list of callback functions. You can specify a custom base URL if needed.
2. Call the `generate` function with the prompt and optional parameters.
3. Handle the result, which can be either an error or the generated text.
4. The `generate` function returns a `Text` response, which you can print or use as needed.

  </TabItem>

 <TabItem value="huggingface">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import qualified Data.Text as T
import Langchain.LLM.Core
import Langchain.LLM.Huggingface 

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = <your-api-key>
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    eRes <- generate huggingface "Explain me Monads in Haskell" Nothing
    case eRes of
      Left err -> putStrLn $ "Chat error: " ++ err
      Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```
In above code:
1. Setup the `Huggingface` LLM with the [provider](https://huggingface.co/docs/inference-providers/index), api key, model name and optional list of callback functions.
2. Call the `generate` function with the prompt and optional parameters.
3. Handle the result, which can be either an error or the generated text.
4. The `generate` function returns a `Text` response, which you can print or use as needed.

  </TabItem>
</Tabs>

### Example of generating response from a chat history

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>
  <TabItem value="ollama">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Langchain.LLM.Ollama (Ollama(..))
import Langchain.LLM.Core
import qualified Data.Text as T
import Data.List.NonEmpty (fromList)

runApp :: IO ()
runApp = do
  let ollamaLLM = Ollama "llama3.2" []  
  let chatHistory = fromList
        [ Message System "Explain everything with a texas accent." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  chatResult <- chat ollamaLLM chatHistory Nothing
  case chatResult of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```

1. Setup the `Ollama` LLM with the model name and optional list of callback functions.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Call the `chat` function with the `chatHistory` and optional parameters.
4. Handle the result, which can be either an error or the generated text.
5. The `chat` function returns a `Text` response, which you can print or use as needed.

:::warning
For Ollama, Make sure the model that you want to use is installed on your local machine; else it will throw error.
:::

:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `messageData`: Optional metadata for the message (A type containing an optional name and optional list of tool calls).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.

`chat` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history. 
:::

</TabItem>

<TabItem value="openai">

  ```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Langchain.LLM.Core as LLM
import qualified Data.Text as T
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  let openAI = OpenAI
        { apiKey = "your-api-key"
        , callbacks = []
        , baseUrl = Nothing
        }
  let chatHistory = fromList
        [ Message System "You are an AI assistant." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  chatResult <- LLM.chat openAI chatHistory Nothing
  case chatResult of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
    ```
In above code:
1. Setup the `OpenAI` LLM with the api key and optional list of callback functions. You can specify a custom base URL if needed.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Call the `chat` function with the `chatHistory` and optional parameters.
4. Handle the result, which can be either an error or the generated text.
5. The `chat` function returns a `Text` response, which you can print or use as needed.

:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `metadata`: Optional metadata for the message (A type containing a optional name and optional list of toolnames) (Currently unstable).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.
`chat` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history.
:::

</TabItem>

<TabItem value="huggingface">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import qualified Data.Text as T
import Langchain.LLM.Core 
import Langchain.LLM.Huggingface
import Data.List.NonEmpty (fromList)

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = <your-api-key>
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    let chatHistory = fromList [ 
            Message System "You are an AI assistant." defaultMessageData
          , Message User "What is functional programming?" defaultMessageData]
    eRes <- chat huggingface chatHistory Nothing
    case eRes of
      Left err -> putStrLn $ "Chat error: " ++ err
      Right response -> putStrLn $ "Chat Response:\n" ++ T.unpack response
```
In above code:
1. Setup the `Huggingface` LLM with the [provider](https://huggingface.co/docs/inference-providers/index), api key, model name and optional list of callback functions.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Call the `chat` function with the `chatHistory` and optional parameters.
4. Handle the result, which can be either an error or the generated text.
5. The `chat` function returns a `Text` response, which you can print or use as needed.
:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `metadata`: Optional metadata for the message (A type containing a optional name and optional list of toolnames) (Currently unstable).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.
`chat` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history.
:::

</TabItem>
</Tabs>

### Example of streaming response

<Tabs
  defaultValue="ollama"
  values={[
    {label: 'Ollama', value: 'ollama'},
    {label: 'OpenAI', value: 'openai'},
    {label: 'Huggingface', value: 'huggingface'}
  ]}>

<TabItem value="ollama">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Langchain.LLM.Ollama (Ollama(..))
import Langchain.LLM.Core
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.NonEmpty (fromList)

main :: IO ()
main = do
  let ollamaLLM = Ollama "llama3.2" []  
  let chatHistory = fromList
        [ Message System "You are an AI assistant." defaultMessageData
        , Message User "What is functional programming?" defaultMessageData
        ]
  let handler = StreamHandler T.putStr (putStrLn "Response complete")
  eRes <- stream ollamaLLM chatHistory handler Nothing 
  case eRes of
    Left err -> putStrLn $ "Chat error: " ++ err
    Right _ -> pure ()
```
In above code:
1. Setup the `Ollama` LLM with the model name and optional list of callback functions.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Create a `StreamHandler` with `onToken` and `onComplete` functions.
4. Call the `stream` function with the `chatHistory`, `StreamHandler` and optional parameters.
5. Handle the result, which can be either an error or unit.
6. The `stream` function returns a unit and the `onToken` function will be called for each token generated. 

:::note
The StreamHandler takes two functions:
1. `onToken`: A function that takes a `Text` and returns `IO ()`. This function will be called for each token generated.
2. `onComplete`: A function that takes a `Text` and returns `IO ()`. This function will be called when the streaming is complete.
:::

:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `metadata`: Optional metadata for the message (A type containing a optional name and optional list of toolnames) (Currently unstable).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.
`stream` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history.
:::
</TabItem>

<TabItem value="openai">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Data.List.NonEmpty (fromList)
import qualified Data.Text.IO as T
import Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI

runApp :: IO ()
runApp = do
    let openAI =
            OpenAI
                { apiKey = "your-api-key"
                , callbacks = []
                , baseUrl = Nothing
                }
    let chatHistory =
            fromList
                [ Message System "You are an AI assistant." defaultMessageData
                , Message User "What is functional programming?" defaultMessageData
                ]
    let streamHandler = StreamHandler {
        onToken = T.putStr,
        onComplete = pure ()
    }
    chatResult <- LLM.stream openAI chatHistory streamHandler Nothing
    case chatResult of
        Left err -> putStrLn $ "Chat error: " ++ err
        Right _ -> pure ()
        ```
In above code:
1. Setup the `OpenAI` LLM with the api key, model name and optional list of callback functions.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Create a `StreamHandler` with `onToken` and `onComplete` functions.
4. Call the `stream` function with the `chatHistory`, `StreamHandler` and optional parameters.
5. Handle the result, which can be either an error or unit.
6. The `stream` function returns a unit and the `onToken` function will be called for each token generated.

:::note
The StreamHandler takes two functions:
1. `onToken`: A function that takes a `Text` and returns `IO ()`. This function will be called for each token generated.
2. `onComplete`: A function that takes a `Text` and returns `IO ()`. This function will be called when the streaming is complete.
:::

:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `metadata`: Optional metadata for the message (A type containing a optional name and optional list of toolnames) (Currently unstable).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.
`stream` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history.
:::

</TabItem>

<TabItem value="huggingface">

```haskell
{-# LANGUAGE OverloadedStrings #-}

module LangchainLib (runApp) where

import Data.List.NonEmpty (fromList)
import qualified Data.Text.IO as T
import Langchain.LLM.Core as LLM
import Langchain.LLM.Huggingface

runApp :: IO ()
runApp = do
    let huggingface =
            Huggingface
                { provider = Cerebras
                , apiKey = "your-api-key"
                , modelName = "llama-3.3-70b"
                , callbacks = []
                }
    let chatHistory = fromList [Message System "You are an AI assistant." defaultMessageData, Message User "What is functional programming?" defaultMessageData]
    let streamHandler =
            StreamHandler
                { onToken = T.putStr
                , onComplete = pure ()
                }
    eRes <- stream huggingface chatHistory streamHandler Nothing
    case eRes of
        Left err -> putStrLn $ "Chat error: " ++ err
        Right _ -> pure ()
        ```
In above code:

1. Setup the `Huggingface` LLM with the [provider](https://huggingface.co/docs/inference-providers/index), api key, model name and optional list of callback functions.
2. Create a `chatHistory` using `fromList` with `Message` constructor.
3. Create a `StreamHandler` with `onToken` and `onComplete` functions.
4. Call the `stream` function with the `chatHistory`, `StreamHandler` and optional parameters.
5. Handle the result, which can be either an error or unit.
6. The `stream` function returns a unit and the `onToken` function will be called for each token generated.

:::note
The StreamHandler takes two functions:
1. `onToken`: A function that takes a `Text` and returns `IO ()`. This function will be called for each token generated.
2. `onComplete`: A function that takes a `Text` and returns `IO ()`. This function will be called when the streaming is complete.
:::

:::note
`Message` constructor takes 3 parameters:
1. `role`: The role of the message sender (System, User, Assistant).
2. `content`: The content of the message (Text).
3. `metadata`: Optional metadata for the message (A type containing a optional name and optional list of toolnames) (Currently unstable).
4. `defaultMessageData`: A default value for the metadata, which can be used if no specific metadata is provided.
`stream` takes a `NonEmpty` list of `Message` as input. The `NonEmpty` type ensures that the list is not empty, which is important for chat history.
:::
</TabItem>
</Tabs>
