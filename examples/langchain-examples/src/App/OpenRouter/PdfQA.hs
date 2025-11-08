{-# LANGUAGE OverloadedStrings #-}

module App.OpenRouter.PdfQA (runApp) where

import Control.Monad (forever, when)
import Control.Monad.Trans.Except
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Langchain.DocumentLoader.PdfLoader
import Langchain.Embeddings.Ollama
import qualified Langchain.Error as Langchain
import Langchain.LLM.OpenAICompatible
import Langchain.PromptTemplate
import Langchain.Retriever.Core
import Langchain.VectorStore.InMemory
import qualified OpenAI.V1.Chat.Completions as CreateChat hiding
  ( ChatCompletionChunk (..)
  , ChatCompletionObject (..)
  )
import qualified OpenAI.V1.Models as Models
import System.Environment

-- Template for system prompt
systemTemplate :: Text
systemTemplate =
  T.unlines
    [ "Use the following pieces of context to answer the user's question."
    , "If you don't know the answer, just say that you don't know, don't try to make up an answer."
    , "ALWAYS return a \"SOURCES\" part in your answer."
    , "The \"SOURCES\" part should be a reference to the source of the document from which you got your answer."
    , "Example response:"
    , "```"
    , "The answer is foo."
    , "SOURCES: xyz"
    , "```"
    , "{context}"
    ]

stripSources :: Message -> Text
stripSources msg =
  let full = content msg
      (beforeSources, _) = T.breakOn "\nSOURCES:" full
   in T.strip beforeSources

-- Main entry point
runApp :: IO ()
runApp = do
  aKey <- T.pack . fromMaybe "api-key" <$> lookupEnv "OPENAI_API_KEY"
  -- Setup
  let openRouter =
        mkOpenRouter
          []
          Nothing
          aKey

  let sourcePdf = PdfLoader "./SOP.pdf"
  let ollamaEmbeddings =
        OllamaEmbeddings
          "nomic-embed-text:latest"
          Nothing
          Nothing
          Nothing

  -- Prepare documents and vector store
  result <- runExceptT $ do
    docs <- ExceptT $ load sourcePdf
    ExceptT $ fromDocuments ollamaEmbeddings docs

  case result of
    Left err -> putStrLn $ "Error loading documents: " <> Langchain.toString err
    Right vectorStore -> do
      let retriever = VectorStoreRetriever vectorStore
      TIO.putStrLn "PDF loaded. You can now ask questions. Type ':quit' to exit.\n"

      -- Interactive loop
      let docToText =
            mconcat . map (\doc -> TL.toStrict (pageContent doc) <> T.pack (show $ metadata doc))
      let promptTemplate = PromptTemplate systemTemplate

      let chatLoop = forever $ do
            TIO.putStr "\n> "
            userInput <- TIO.getLine
            when (T.strip userInput == ":quit") $ do
              TIO.putStrLn "Goodbye!"
              fail "User exited"

            response <- runExceptT $ do
              relevantDocs <- ExceptT $ _get_relevant_documents retriever userInput
              let context = docToText relevantDocs
              sysPrompt <-
                ExceptT . pure $
                  renderPrompt promptTemplate (HM.fromList [("context", context)])
              let sysMsg = Message System sysPrompt defaultMessageData
              let userMsg = Message User userInput defaultMessageData
              ExceptT $
                chat
                  openRouter
                  (sysMsg <| (userMsg :| []))
                  ( Just $
                      CreateChat._CreateChatCompletion
                        { CreateChat.model = Models.Model "qwen/qwen3-coder:free"
                        }
                  )

            case response of
              Left err -> putStrLn $ "Error: " <> Langchain.toString err
              Right r -> TIO.putStrLn $ stripSources r

      chatLoop
