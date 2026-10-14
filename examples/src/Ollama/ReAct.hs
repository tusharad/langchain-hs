{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ollama.ReAct (runApp) where

import Control.Monad.Except (runExceptT)
import qualified Data.Text.IO as T
import Langchain.Prelude
import Langchain.Provider.Ollama

runApp :: IO ()
runApp = do
  writeFile "/tmp/sample_task.txt" "Production note: Always use secure and tested tools."
  o <- newOllama "qwen3.5:2b" defaultConfig
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
