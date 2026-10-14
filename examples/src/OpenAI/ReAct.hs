{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module OpenAI.ReAct (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  writeFile "/tmp/sample_task.txt" "Important production note: Keep tools safe and reliable."
  o <- getOpenRouterModel defaultModelName
  let policy = defaultSecurityPolicy
      tools = [readFileTool Nothing policy, findFilesTool policy]
      agent = defaultReActAgent o tools
      prompt =
        [ userMessage
            "Read the contents of /tmp/sample_task.txt using read_file, then summarize its note."
        ]

  res <- runExceptT $ runReActAgent agent prompt
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right msg -> T.putStrLn $ extractMessageText msg
