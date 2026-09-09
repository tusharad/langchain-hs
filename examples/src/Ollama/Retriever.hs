{-# LANGUAGE OverloadedStrings #-}

module Ollama.Retriever (runApp) where

import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Langchain.Prelude

runApp :: IO ()
runApp = do
  cb <- newCallbackManager
  (handler, logsVar) <- newLoggingCallbackHandler "RetrieverLogger"
  registerHandler cb handler

  let docs =
        [ Document
            "Haskell features pure functional programming, strong static typing, and immutability."
            mempty
        , Document "GHC-9.8 introduces improved compiler error messages and new typechecker features." mempty
        , Document "Reciprocal Rank Fusion fuses ranked results from sparse and dense retrievers." mempty
        ]
      bm25 = newBM25Index docs

  res <- runLangchainTIO $ do
    let embed = OllamaEmbeddings "nomic-embed-text:latest" Nothing Nothing Nothing
    vecStore <- fromDocuments embed docs
    let vecSearch q k = fromRight [] <$> runLangchainTIO (similaritySearch vecStore q k)
        hybrid = newHybridRetriever bm25 vecSearch

    matchedBM25 <- retrieveWithCallbacks cb "BM25" bm25 "GHC-9.8"
    liftIO $ T.putStrLn $ "BM25 match: " <> firstDoc matchedBM25

    matchedHybrid <- retrieveWithCallbacks cb "Hybrid" hybrid "pure functional language features"
    liftIO $ T.putStrLn $ "Hybrid match: " <> firstDoc matchedHybrid

    o <- newOllama "gemma3" defaultConfig
    ask_ o cb "Explain the compiler improvements in GHC-9.8." (firstDoc matchedBM25)

  case res of
    Left err -> T.putStrLn $ "Error: " <> errorMessage err
    Right () -> pure ()

  logs <- getCallbackLogs logsVar
  T.putStrLn "\n--- Callback Logs ---"
  mapM_ T.putStrLn logs

ask_ :: Ollama -> CallbackManager -> Text -> Text -> LangchainT IO ()
ask_ llm cb query context = do
  start <- liftIO getCurrentTime
  let prompt = "Context: " <> context <> "\nQuestion: " <> query
  liftIO $ dispatchEvent cb (OnLLMStart "gemma3" [prompt] start)
  resp <- invoke llm [userMessage prompt] Nothing
  end <- liftIO getCurrentTime
  let durMicros = round (diffUTCTime end start * 1000000)
      ans = extractMessageText resp
  liftIO $ dispatchEvent cb (OnLLMEnd "gemma3" ans durMicros end)
  liftIO $ T.putStrLn $ "AI: " <> ans

firstDoc :: [Document] -> Text
firstDoc [] = ""
firstDoc (d : _) = TL.toStrict (pageContent d)
