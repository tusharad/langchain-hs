{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : Cortex-Agent CLI Application
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental
-}
module Main (main) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

import Cortex.Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["research", topic] -> do
      putStrLn $ "=== 🧠 Cortex-Agent Autonomous Deep Research: " ++ topic ++ " ==="
      model <- newOllama "qwen3.5:9b"
      let mockSearch _ = pure ["https://en.wikipedia.org/wiki/Haskell", "https://wiki.haskell.org/Introduction"]
      eRes <- runExceptT $ runAutonomousResearch model defaultScraperConfig mockSearch (T.pack topic)
      case eRes of
        Left err -> putStrLn $ "Research Error: " ++ show err
        Right report -> do
          putStrLn "\n=== Generated Deep Research Report ===\n"
          TIO.putStrLn (reportMarkdown report)

    ["brain", "create", name] -> do
      store <- newBrainStore "cortex.db"
      brain <- createBrain store (defaultBrainConfig (T.pack name))
      putStrLn $ "Created Brain: " ++ T.unpack (unBrainId (brainId brain))

    _ -> do
      putStrLn "Cortex-Agent Cognitive CLI"
      putStrLn "Usage:"
      putStrLn "  cortex-cli research <topic>     - Run autonomous deep research"
      putStrLn "  cortex-cli brain create <name>  - Create a new second-brain"
