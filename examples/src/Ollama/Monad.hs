{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Ollama.Monad (runApp) where

import Langchain.Prelude
import qualified Data.Text.IO as T
import Langchain.Provider.Ollama

runApp :: IO ()
runApp = do
    o <- newOllama ""
    let msg = [userMessage "Write a poem about functional programming"]
    res <- runLangchainTIO $ do
       _defaultConf <- askConfig
       let chatReq = chatRequest _ _
       invoke o [] (Just $ chatReq)
    case res of
      Left err -> T.putStrLn $ errorMessage err
      Right m -> T.putStrLn $ extractMessageText m
