{-# LANGUAGE OverloadedStrings #-}
module Ollama.RAG (runApp) where

import Langchain.Prelude
import Control.Monad.Except (throwError)
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as T
import Langchain.Embeddings.Ollama
import Langchain.PromptTemplate.Prompt
import qualified Data.Map as Map

runApp :: IO ()
runApp = do
    let systemPrompt = "Answer the question based on the Context provided it to you."
        userQuestion = "Compare the feautres of Langchain-Rust and Langchain-Haskell."
    let fPath = FileLoader "/Users/tusharadhatrao/work/langchain-clients/langchain-hs/README.md"
    res <- runLangchainTIO $ do
        docs <- load fPath
        let ollamaEmbed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
        vs1 <- fromDocuments ollamaEmbed docs
        relevantDocs <- similaritySearch vs1 userQuestion 1
        o <- newOllama "qwen3.5:2b" defaultConfig 
        let ragTemplate = "{userQuestion} CONTEXT: {context}"
        let x = (T.toStrict . pageContent) $ mconcat relevantDocs
        let vars = Map.fromList [("userQuestion", userQuestion) , ("context", x)]
        let eFinalQ = renderFStringTemplate vars  ragTemplate
        case eFinalQ of
          Right finalQ -> do
            let msgs = zipWith id
                            [systemMessage, userMessage]
                            [systemPrompt, finalQ]
            let chatReq = withOptions (defaultOptions {optNumCtx = Just 100096}) (chatRequestFor o msgs)
            invoke o msgs (Just chatReq)
          Left _ -> throwError $ internalError "Rendering of vars failed" Nothing Nothing
    case res of
      Left err -> T.putStrLn $ errorMessage err
      Right r -> T.putStrLn $ extractMessageText r
