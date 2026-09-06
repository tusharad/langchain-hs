module Common.PromptTemplate (runApp) where

import Langchain.PromptTemplate.String

runApp :: IO ()
runApp = do
  let _ = renderFStringTemplate
  undefined
