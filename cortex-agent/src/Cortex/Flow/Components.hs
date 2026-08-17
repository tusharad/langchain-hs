{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Cortex.Flow.Components
Description : Declarative Dynamic Flow Component Registry (Langflow style)
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides standard declarative node components for dynamic JSON flows including
Prompt templates, LLM invocations, Brain knowledge retrievers, Web scrapers, and Evaluators.
-}
module Cortex.Flow.Components
  ( buildCortexComponentRegistry
  , promptComponent
  , llmComponent
  , brainRetrieverComponent
  , scraperComponent
  ) where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value (..), toJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Cortex.Knowledge.Retriever (BrainRetriever (..), queryBrain)
import Cortex.Research.Scraper (ScrapedSource (..), defaultScraperConfig, scrapeUrl)
import Langchain.Core.Error (LangchainError, agentError)
import Langchain.Core.Model (ChatModel, Role (..), extractMessageText, invoke, textMessage)
import Langchain.DocumentLoader.Core (Document (..))
import Langchain.Graph.DynamicFlow (ComponentRegistry, FlowNode (..), NodeExecutor)

-- | Build the default Cortex component registry for dynamic flow execution
buildCortexComponentRegistry
  :: (ChatModel model, MonadIO m, MonadError LangchainError m)
  => model
  -> Maybe (BrainRetriever model)
  -> ComponentRegistry m
buildCortexComponentRegistry model mbBrainRetriever =
  Map.fromList
    [ ("prompt", promptComponent)
    , ("llm", llmComponent model)
    , ("brain_retriever", brainRetrieverComponent mbBrainRetriever)
    , ("web_scraper", scraperComponent)
    ]

-- | Prompt template node executor
promptComponent :: (MonadIO m) => NodeExecutor m
promptComponent FlowNode {..} inputs = do
  let template = case Map.lookup "template" nodeParams of
        Just (String t) -> t
        _ -> case Map.lookup "template" inputs of
          Just (String t) -> t
          _ -> "{input}"
  let inputVal = case Map.lookup "input" inputs of
        Just (String s) -> s
        _ -> ""
  let rendered = T.replace "{input}" inputVal template
  pure $ Map.singleton "prompt_text" (String rendered)

-- | LLM chat invocation node executor
llmComponent :: (ChatModel model, MonadIO m, MonadError LangchainError m) => model -> NodeExecutor m
llmComponent model _ inputs = do
  let promptStr = case Map.lookup "prompt_text" inputs of
        Just (String s) -> s
        _ -> case Map.lookup "input" inputs of
          Just (String s) -> s
          _ -> "Hello"
  let msg = textMessage User promptStr
  aiMsg <- invoke model [msg] Nothing
  let outText = extractMessageText aiMsg
  pure $ Map.fromList [("response", String outText), ("text", String outText)]

-- | Brain Retriever node executor
brainRetrieverComponent :: (ChatModel model, MonadIO m, MonadError LangchainError m) => Maybe (BrainRetriever model) -> NodeExecutor m
brainRetrieverComponent (Just br) _ inputs = do
  let queryText = case Map.lookup "query" inputs of
        Just (String q) -> q
        _ -> case Map.lookup "text" inputs of
          Just (String q) -> q
          _ -> ""
  docs <- queryBrain br queryText
  let docTexts = [TL.toStrict (pageContent d) | d <- docs]
  pure $ Map.fromList
    [ ("documents", toJSON docTexts)
    , ("context", String (T.intercalate "\n---\n" docTexts))
    , ("count", Number (fromIntegral (length docs)))
    ]
brainRetrieverComponent Nothing _ _ =
  pure $ Map.singleton "error" (String "No BrainRetriever configured in flow environment")

-- | Web scraper node executor
scraperComponent :: (MonadIO m) => NodeExecutor m
scraperComponent FlowNode {..} inputs = do
  let targetUrl = case Map.lookup "url" inputs of
        Just (String u) -> u
        _ -> case Map.lookup "url" nodeParams of
          Just (String u) -> u
          _ -> ""
  if T.null targetUrl
    then pure $ Map.singleton "error" (String "Missing URL input for web_scraper node")
    else do
      mbSrc <- scrapeUrl defaultScraperConfig targetUrl
      case mbSrc of
        Just src ->
          pure $ Map.fromList
            [ ("content", String (sourceContent src))
            , ("title", String (sourceTitle src))
            , ("word_count", Number (fromIntegral (sourceWordCount src)))
            ]
        Nothing ->
          pure $ Map.singleton "error" (String ("Failed to scrape URL: " <> targetUrl))
