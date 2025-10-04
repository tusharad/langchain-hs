module Langchain.Utils (showText) where

import Data.Text (Text, pack)

showText :: Show a => a -> Text
showText = pack . show
