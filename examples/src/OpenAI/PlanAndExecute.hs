{-# LANGUAGE OverloadedStrings #-}

module OpenAI.PlanAndExecute (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let tools = [shellTool]
      executor = createReActAgent o tools
      agent = newPlanAndExecuteAgent o executor Nothing
      goal =
        "Use shell commands to check the operating system name (uname -s) and architecture (uname -m), then summarize the host platform."
  res <- runExceptT $ runPlanAndExecute agent goal
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right ans -> T.putStrLn ans
