{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Test.Tasty.Bench

import Langchain.Core.Error
import Langchain.Core.Model
import Langchain.Core.Runnable
import Langchain.Graph.StateGraph
import Langchain.TextSplitter.Character
import Langchain.VectorStore.InMemory (cosineSimilarity, dotProduct)

main :: IO ()
main = defaultMain
  [ bgroup "RunnableTree"
      [ bench "interpret-depth-1" $ nfIO $
          runExceptT $ interpret (runLambda (\t -> pure $ Right (t <> ("!" :: T.Text)))) "hello"
      , bench "interpret-depth-5" $ nfIO $
          let p = runLambda (\t -> pure $ Right (t <> "a"))
                    |>> runLambda (\t -> pure $ Right (t <> "b"))
                    |>> runLambda (\t -> pure $ Right (t <> "c"))
                    |>> runLambda (\t -> pure $ Right (t <> "d"))
                    |>> runLambda (\t -> pure $ Right (t <> "e"))
           in runExceptT $ interpret p "hello"
      , bench "interpret-depth-20" $ nfIO $
          let step = runLambda (\t -> pure $ Right (t <> "."))
              p = foldr (|>>) Id (replicate 20 step)
           in runExceptT $ interpret p "hello"
      ]
  , bgroup "Message JSON Serialization"
      [ bench "encode-text-message" $ nf encode (userMessage "What is functional programming?")
      , bench "decode-text-message" $ nf (decode :: LBS.ByteString -> Maybe Message) (encode $ userMessage "What is functional programming?")
      ]
  , bgroup "StateGraph"
      [ bench "compile-and-run-3-nodes" $ nfIO $ do
          let g = addEdge "n1" "n2"
                $ addEdge "n2" "n3"
                $ addEdge "n3" endNodeId
                $ addNode "n1" (\s -> pure $ Right (s <> ("-1" :: T.Text)))
                $ addNode "n2" (\s -> pure $ Right (s <> "-2"))
                $ addNode "n3" (\s -> pure $ Right (s <> "-3"))
                $ emptyStateGraph replaceFieldReducer
          case compileGraph g of
            Left _ -> pure (Left $ internalError "compile error" Nothing Nothing)
            Right cg -> runExceptT $ runGraph cg "n1" "start"
      ]
  , bgroup "Vector Operations"
      [ bench "dotProduct-1536-dims" $ nf (dotProduct (replicate 1536 0.1)) (replicate 1536 0.2)
      , bench "cosineSimilarity-1536-dims" $ nf (cosineSimilarity (replicate 1536 0.1)) (replicate 1536 0.2)
      ]
  , bgroup "TextSplitter"
      [ bench "splitText-100kb" $
          let text = TL.replicate 1000 "This is a paragraph of text meant to simulate document chunking in RAG pipelines.\n\n"
           in nf (splitText defaultCharacterSplitterOps) text
      ]
  ]
