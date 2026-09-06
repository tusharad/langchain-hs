{-# LANGUAGE OverloadedStrings #-}

module Ollama.PlanAndExecute (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude

runApp :: IO ()
runApp = do
  o <- newOllama "qwen3.5:2b" defaultConfig
  let tools = [shellTool]
      executor = createReActAgent (bindTools tools o) tools
      agent = newPlanAndExecuteAgent o executor Nothing
      goal =
        "Use shell commands to check the operating system name (uname -s) and architecture (uname -m), then summarize the host platform."
  res <- runExceptT $ runPlanAndExecute agent goal
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
