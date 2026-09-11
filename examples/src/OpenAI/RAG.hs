{-# LANGUAGE OverloadedStrings #-}

module OpenAI.RAG (runApp) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as T
import Langchain.Prelude
import Langchain.PromptTemplate.Prompt
import OpenAI.Common (defaultModelName, getOpenRouterEmbeddings, getOpenRouterModel)

runApp :: IO ()
runApp = do
  let systemPrompt = "Answer the question based on the Context provided it to you."
      userQuestion = "Compare the features of Langchain-Rust and Langchain-Haskell."
  let fPath = FileLoader "README.md"
  res <- runLangchainT () $ do
    docs <- load fPath
    openAIEmbed <- liftIO $ getOpenRouterEmbeddings "text-embedding-3-small"
    vs1 <- fromDocuments openAIEmbed docs
    relevantDocs <- similaritySearch vs1 userQuestion 1
    o <- liftIO $ getOpenRouterModel defaultModelName
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
        invoke o msgs Nothing
      Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right r -> T.putStrLn $ extractMessageText r
