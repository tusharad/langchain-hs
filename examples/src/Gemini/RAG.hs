{-# LANGUAGE OverloadedStrings #-}

module Gemini.RAG (runApp) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Gemini.Common (defaultGeminiModelName, getGeminiEmbeddings, getGeminiModel)
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt

runApp :: IO ()
runApp = do
  T.putStrLn "--- Gemini RAG with GeminiEmbeddings ---"
  let systemPrompt = "Answer the question based on the Context provided to you."
      userQuestion = "What are the features of LangChain Haskell?"
  let fPath = FileLoader "README.md"
  res <- runLangchainT () $ do
    docs <- load fPath
    geminiEmbed <- liftIO getGeminiEmbeddings
    vs1 <- fromDocuments geminiEmbed docs
    relevantDocs <- similaritySearch vs1 userQuestion 1
    g <- liftIO $ getGeminiModel defaultGeminiModelName
    let ragTemplate = "{userQuestion} CONTEXT: {context}"
    let x = (T.toStrict . pageContent) $ mconcat relevantDocs
    let vars = Map.fromList [("userQuestion", userQuestion), ("context", x)]
    let eFinalQ = renderFStringTemplate vars ragTemplate
    case eFinalQ of
      Right finalQ -> do
        let msgs =
              zipWith
                id
                [systemMessage, userMessage]
                [systemPrompt, finalQ]
        invoke g msgs Nothing
      Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
