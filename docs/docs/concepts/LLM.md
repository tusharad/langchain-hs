---
sidebar_position: 1
---

# LLM

The `LLM` typeclass is the cornerstone of langchain-hs. It provides a unified interface for:

 - Text generation
 - Chat conversations
 - Streaming responses
 - Custom parameter handling

Every type that has LLM instance, can call generate, chat and stream.

## Supported Integrations

At this moment, following integrations available,

 - Ollama
 - OpenAI
 - HuggingFace
 - Deepseek
 - OpenAI compatible
 - More to come...

## A closer look at LLM Typeclass

```haskell
class LLM m where
  generate :: m -- ^ The type of the language model instance.
    -> Text -- ^ The prompt to send to the model.
    -> Maybe (LLMParams m) -- ^ Optional configuration parameters.
    -> IO (Either String Text)

  chat :: m 
    -> ChatMessage -- ^ A non-empty list of messages to send to the model.
    -> Maybe (LLMParams m) -- ^ Optional configuration parameters.
    -> IO (Either String Text) -- ^ The result of the chat, either an error or the response text.

  stream :: m -> ChatMessage -> StreamHandler -> Maybe (LLMParams m) -> IO (Either String ())
```

As you can see, the LLM typeclass has three main functions: `generate`, `chat`, and `stream`. Each of these functions takes an instance of the LLM typeclass, a prompt or message, and optional parameters. The `generate` and `chat` function returns a generated text response. The `stream` function allows for streaming responses from the model.

The `LLMParams` type is used to specify optional parameters for the LLM instance. This allows for customization of the model's behavior, such as temperature, max tokens, and other settings.

The `StreamHandler` type is used to handle streaming responses from the model. This allows for real-time processing of the model's output as it is generated.

The `LLM` typeclass is designed to be flexible and extensible, allowing for easy integration of new language models and custom behavior. By implementing the `LLM` typeclass for a specific model, you can take advantage of the unified interface and easily switch between different models without changing your code.

## The ChatMessage type

```haskell
type ChatMessage = NonEmpty Message
```

The `ChatMessage` type is a non-empty list of `Message` types. This ensures that there is always at least one message in the chat conversation.

```
data Message = Message
  { role :: Role
  , content :: Text
  , messageData :: MessageData
  }
```

The `Message` type represents a single message in the chat conversation. It has three fields:
  - `role`: The role of the message sender (Possible values are User, Assistant, System, Tool, Developer, Function).
  - `content`: The content of the message (Text).
  - `messageData`: Additional data associated with the message, such as name or tool calls. You can use `defaultMessageData` to create a default instance of `MessageData` with no additional data.

## Writing your own LLM

Let's take an example of writing an instance for Deepseek LLM.

```haskell
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module LangchainLib (runApp) where

import Langchain.LLM.Core
import Data.Text (Text)

data Deepseek = Deepseek
  { dsApiKey    :: Text      -- ^ Your Deepseek API key
  , dsEndpoint  :: Text      -- ^ Base URL of the Deepseek API
  , dsModel     :: Text      -- ^ Model name, e.g. "deepseek-base"
  }

data DeepseekParams = DeepseekParams
  { dsTemperature :: Maybe Double  -- ^ Sampling temperature
  , dsMaxTokens   :: Maybe Int     -- ^ Maximum tokens in output
  }

instance LLM Deepseek where
  type LLMParams Deepseek = DeepseekParams
 
  generate Deepseek{..} prompt mParams = do
    let params = fromMaybe defaultDeepseekParams mParams
        req    = makeRequest dsEndpoint prompt params
    response <- httpBS req
    let status = getResponseStatusCode response
        body   = TE.decodeUtf8 $ getResponseBody response
    return $ if status >= 200 && status < 300
      then Right body
      else Left $ "Deepseek error: HTTP " ++ show status ++ "; " ++ show body
```

In this example, we define a `Deepseek` data type that represents the Deepseek LLM. We also define a `DeepseekParams` data type for optional parameters. The `generate` function constructs an HTTP request to the Deepseek API and handles the response.
The `LLM` typeclass is implemented for the `Deepseek` type, allowing it to be used with the unified interface provided by langchain-hs. 

```note
LLM typeclass takes ChatMessage as input for chat and stream functions. Where Message takes Role, Text and MessageData as input. The LLM client may take different Role or MessageData as input. For example, Ollama does not take Developer or Function role. Hence, it is recommended to convert the Message/Role to the LLM client specific type before calling the LLM client.
```

## OpenAI Compatible Services

For services that implement the **OpenAI API specification**, langchain-hs provides a flexible integration mechanism through the `OpenAICompatible` type. 
This enables seamless interaction with local or custom servers like **LMStudio**, **Llama.cpp**, or any OpenAI-compatible endpoint.

---

### Key Features

- **Universal Adapter**: Reuse OpenAI client logic for any compatible API
- **Custom Endpoints**: Point to any OpenAI-compatible service
- **Local Model Support**: Easily integrate with local inference servers
- **Full LLM Typeclass Support**: Access to `generate`, `chat`, and `stream` operations

---

#### Creating Clients

Use the helper functions to quickly set up common configurations:

```haskell
-- Create an LMStudio client
let lmStudioClient = mkLMStudio 
  "llama3-8b"              -- Model name
  []                      -- Callbacks (empty list)
  Nothing                 -- Custom base URL (uses default if omitted)
  Nothing                 -- API key (optional for local servers)

-- Create a Llama.cpp client
let llamaCppClient = mkLlamaCpp 
  "mistral-7b"             -- Model name
  []                       -- Callbacks
  (Just "http://api.example.com/v1")  -- Custom server URL
  Nothing                 -- API key
```

#### Custom Configuration

For full control, construct the type directly:

```haskell
let customClient = OpenAICompatible
  { apiKey = Just "your-api-key"
  , modelName = "custom-mistral"
  , callbacks = [stdOutCallback]  -- Example with logging callback
  , baseUrl = Just "https://my-ai-server.com/openai/v1"
  , defaultBaseUrl = "http://localhost:8080/v1"
  , providerName = "MyCustomProvider"
  }
```

---


#### Data Type Fields

| Field | Description | Example |
|-------|-------------|---------|
| `apiKey` | Optional authentication token | `"sk-..."` |
| `modelName` | Model identifier | `"llama3-8b"` |
| `callbacks` | Event handlers | `[consoleCallback]` |
| `baseUrl` | Custom API endpoint | `"http://localhost:1234/v1"` |
| `defaultBaseUrl` | Fallback URL | `"http://localhost:8080/v1"` |
| `providerName` | Service identifier | `"LMStudio"` |

#### Helper Functions

- `toOpenAI`: Converts `OpenAICompatible` to standard `OpenAI` type for internal processing
- `mkLMStudio`: Specialized constructor for LMStudio servers
- `mkLlamaCpp`: Specialized constructor for Llama.cpp servers

---

#### Implementation Details

The `OpenAICompatible` type leverages the existing OpenAI implementation:

```haskell
instance LLM.OpenAICompatible where
  type LLMParams OpenAICompatible = OpenAI.OpenAIParams
  generate model = LLM.generate (toOpenAI model)
  chat model = LLM.chat (toOpenAI model)
  stream model = LLM.stream (toOpenAI model)

-- Internal/OpenAI.hs
request_ <-
    parseRequest $
      fromMaybe "https://api.openai.com/v1" (baseUrl r)
        <> "/chat/completions"
```

This architecture allows:

1. Reuse of OpenAI's request/response handling
2. Consistent parameter management
3. Unified streaming capabilities
4. Seamless callback integration

---

#### Requirements

When using OpenAI-compatible services:

1. Ensure the server is running and accessible
2. Verify the API endpoint matches OpenAI's specification
3. Check model compatibility
4. Configure CORS and authentication appropriately

For local servers, common default ports include:
- LMStudio: `1234`
- Llama.cpp: `8080`
