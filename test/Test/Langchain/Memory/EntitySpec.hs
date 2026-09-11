{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Memory.EntitySpec (tests) where

import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Langchain.Core.Model (userMessage)
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.Entity
import Test.Langchain.Provider.Mock (newMockModel)

tests :: TestTree
tests =
  testGroup
    "Langchain.Memory.EntitySpec"
    [ testCase "EntityMemory extracts and injects entities into conversation" $ do
        let mockModel = newMockModel "User: Likes Haskell and functional programming\nProject: Langchain-HS"
        mem <- newEntityMemory mockModel []
        res <- runExceptT $ do
          addMessage mem (userMessage "I am working on Langchain-HS and love functional programming")
          entities <- getEntities mem
          allMsgs <- messages mem
          pure (entities, allMsgs)
        case res of
          Left err -> assertFailure ("EntityMemory failed: " ++ show err)
          Right (entities, msgs) -> do
            Map.lookup "User" entities @?= Just "Likes Haskell and functional programming"
            Map.lookup "Project" entities @?= Just "Langchain-HS"
            assertBool "Includes system message with entities" (length msgs >= 2)
    ]
