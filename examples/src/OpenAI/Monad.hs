{-# LANGUAGE OverloadedStrings #-}

module OpenAI.Monad (runApp) where

import Data.Aeson (object, (.=))
import qualified Data.Text.IO as T
import Langchain.Prelude
import OpenAI.Common (defaultModelName, getOpenRouterModel)

runApp :: IO ()
runApp = do
  o <- getOpenRouterModel defaultModelName
  let msg = [userMessage "Write a poem about functional programming"]
  res <- runLangchainT () $ do
    let chatReq =
          object
            [ "temperature" .= (0.7 :: Double)
            , "top_p" .= (0.9 :: Double)
            , "max_tokens" .= (2048 :: Int)
            ]
    invoke o msg (Just chatReq)
  case res of
    Left err -> T.putStrLn $ errorMessage err
    Right m -> T.putStrLn $ extractMessageText m
