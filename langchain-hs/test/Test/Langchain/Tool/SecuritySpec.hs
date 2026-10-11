{-# LANGUAGE OverloadedStrings #-}

module Test.Langchain.Tool.SecuritySpec (tests) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Langchain.Tool (createTool, toolExecute)
import Langchain.Tool.Security
  ( SecurityPolicy (..)
  , ToolTier (..)
  , defaultSecurityPolicy
  , readOnlySecurityPolicy
  , validateCommandSafety
  , validateSandboxPath
  , withSecurity
  , workspaceSandboxPolicy
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Langchain.Tool.Security"
    [ testCase "validateSandboxPath prevents path traversal outside sandbox" $ do
        withSystemTempDirectory "sandbox-test" $ \dir -> do
          let policy = workspaceSandboxPolicy dir
          -- Valid path within sandbox
          eValid <- validateSandboxPath policy (dir </> "file.txt")
          case eValid of
            Left err -> assertFailure $ "Expected valid path: " ++ show err
            Right p -> assertBool "Path cleaned" (T.pack dir `T.isPrefixOf` T.pack p)

          -- Invalid path outside sandbox
          eInvalid <- validateSandboxPath policy (dir </> ".." </> "outside.txt")
          case eInvalid of
            Left _ -> pure ()
            Right p -> assertFailure $ "Expected path traversal to be blocked: " ++ p
    , testCase "validateCommandSafety blocks destructive and fork-bomb commands" $ do
        let policy = defaultSecurityPolicy
        case validateCommandSafety policy "rm -rf /" of
          Left _ -> pure ()
          Right () -> assertFailure "Expected 'rm -rf /' to be blocked"

        case validateCommandSafety policy ":(){ :|:& };:" of
          Left _ -> pure ()
          Right () -> assertFailure "Expected fork bomb to be blocked"

        case validateCommandSafety policy "ls -la src" of
          Left err -> assertFailure $ "Safe command should not be blocked: " ++ show err
          Right () -> pure ()
    , testCase "withSecurity enforces tier restrictions (read-only blocks writes)" $ do
        let readOnlyPolicy = readOnlySecurityPolicy
            dummyWriteTool =
              withSecurity TierWorkspaceWrite readOnlyPolicy $
                createTool "dummy_write" "Dummy write" (object []) (\_ -> pure $ Right "written")
        res <- toolExecute dummyWriteTool (object [])
        case res of
          Left _ -> pure () -- Blocked
          Right _ -> assertFailure "Expected write tool to be blocked by read-only policy"
    ]
