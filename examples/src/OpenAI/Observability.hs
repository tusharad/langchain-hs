{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Observability (runApp) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  tracer <- newOTelTracer (Just "trace-openai")
  res <- runExceptT $ do
    chat_ o tracer "Why is functional programming useful?"
    spans <- getSpans tracer
    liftIO $ mapM_ printSpan spans
    json <- exportSpansJson tracer
    liftIO $ do
      T.putStrLn "Spans JSON:"
      T.putStrLn json
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right _ -> pure ()

chat_ :: OpenAI -> OTelTracer -> Text -> ExceptT LangchainError IO ()
chat_ model_ tracer prompt = do
  sp <-
    startSpan
      tracer
      "llm_invoke"
      Nothing
      ClientSpan
      ( Map.fromList
          [ ("provider", "openai-compatible")
          , ("model", defaultModelName)
          , ("input_length", T.pack (show (T.length prompt)))
          ]
      )
  res <- invoke model_ [userMessage prompt] Nothing
  let answer = extractMessageText res
  addSpanAttribute tracer (spanId sp) "output_length" (T.pack (show (T.length answer)))
  endSpan tracer (spanId sp) StatusOk
  liftIO $ T.putStrLn $ "AI: " <> answer

printSpan :: Span -> IO ()
printSpan sp = do
  T.putStrLn $ "Span: " <> spanName sp <> " (" <> spanId sp <> ")"
  T.putStrLn $ "Trace ID: " <> spanTraceId sp
  T.putStrLn $ "Duration: " <> maybe "0" (T.pack . show) (spanDurationMicros sp) <> "us"
  mapM_ (\(k, v) -> T.putStrLn $ "  " <> k <> ": " <> v) (Map.toList (spanAttributes sp))
