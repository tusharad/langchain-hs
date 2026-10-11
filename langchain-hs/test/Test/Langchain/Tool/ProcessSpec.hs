{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.ProcessSpec (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Tool (toolExecute)
import Langchain.Tool.Process
  ( commandKillTool
  , commandRunTool
  , commandStatusTool
  , newProcessManager
  )
import Langchain.Tool.Security (defaultSecurityPolicy)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.Process"
    [ testCase "commandRunTool executes short command synchronously" $ do
        pm <- newProcessManager
        let policy = defaultSecurityPolicy
        res <-
          toolExecute
            (commandRunTool pm policy)
            (object ["command" .= ("echo 'Hello from Process!'" :: Text), "timeout_ms" .= (2000 :: Int)])
        case res of
          Left err -> assertFailure $ "Command failed: " ++ show err
          Right txt -> do
            assertBool "Contains command output" ("Hello from Process!" `T.isInfixOf` txt)
            assertBool "Indicates exit code 0" ("exit code 0" `T.isInfixOf` txt)
    , testCase "commandRunTool transitions long command to background after timeout" $ do
        pm <- newProcessManager
        let policy = defaultSecurityPolicy
        -- Run sleep 2 with 200ms timeout
        res <-
          toolExecute
            (commandRunTool pm policy)
            (object ["command" .= ("sleep 2" :: Text), "timeout_ms" .= (200 :: Int)])
        case res of
          Left err -> assertFailure $ "Command launch failed: " ++ show err
          Right txt -> do
            assertBool
              "Mentions background execution"
              ("background" `T.isInfixOf` txt || "proc-" `T.isInfixOf` txt)

            -- Check status using commandStatusTool
            sRes <-
              toolExecute
                (commandStatusTool pm policy)
                (object ["process_id" .= ("proc-1" :: Text)])
            case sRes of
              Left err -> assertFailure $ "Status check failed: " ++ show err
              Right sTxt -> assertBool "Has process status output" ("proc-1" `T.isInfixOf` sTxt)

            -- Kill background process
            kRes <-
              toolExecute
                (commandKillTool pm policy)
                (object ["process_id" .= ("proc-1" :: Text)])
            case kRes of
              Left err -> assertFailure $ "Kill failed: " ++ show err
              Right kTxt -> assertBool "Terminated successfully" ("terminated" `T.isInfixOf` T.toLower kTxt)
    ]
