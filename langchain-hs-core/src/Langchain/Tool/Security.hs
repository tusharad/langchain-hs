{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Langchain.Tool.Security
Description : Security, permission tiers, sandbox validation, and HITL hooks
Copyright   : (c) 2025-2026 Tushar Adhatrao
License     : MIT
Maintainer  : Tushar Adhatrao <tusharadhatrao@gmail.com>
Stability   : experimental

Provides tiered permission policies for tools (ReadOnly, WorkspaceWrite, Execution, Admin),
filesystem path sandboxing, sensitive credential filtering, dangerous command prevention,
and Human-in-the-Loop (HITL) approval hooks.
-}
module Langchain.Tool.Security
  ( -- * Permission Tiers
    ToolTier (..)

    -- * Security Policy
  , SecurityPolicy (..)
  , defaultSecurityPolicy
  , readOnlySecurityPolicy
  , workspaceSandboxPolicy
  , unrestrictedSecurityPolicy

    -- * Approval & HITL
  , ApprovalRequest (..)
  , ApprovalDecision (..)

    -- * Validation Functions
  , checkTierPermission
  , validateSandboxPath
  , isSensitivePath
  , validateCommandSafety

    -- * Tool Security Combinator
  , withSecurity
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, normalise, pathSeparator, takeDirectory, takeFileName, (</>))

import Langchain.Error (LangchainError, toolError)
import Langchain.Tool (Tool (..))

-- | Tier levels representing the risk and side-effects of tool execution.
data ToolTier
  = -- | Safe, observational queries (find, grep, read, list, status)
    TierReadOnly
  | -- | Modifies files within workspace sandbox (write, edit, rollback)
    TierWorkspaceWrite
  | -- | Spawns system processes and shell commands
    TierExecution
  | -- | Unrestricted or system-level modifications
    TierAdmin
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, ToJSON, FromJSON)

-- | Human-in-the-Loop approval request structure.
data ApprovalRequest = ApprovalRequest
  { reqToolName :: !Text
  , reqTier :: !ToolTier
  , reqArguments :: !Value
  , reqDescription :: !Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Decision result from an approval hook.
data ApprovalDecision
  = Approved
  | Denied !Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Security policy controlling tool authorization and sandboxing bounds.
data SecurityPolicy = SecurityPolicy
  { policyAllowedTiers :: !(Set.Set ToolTier)
  -- ^ Set of authorized tiers
  , policyWorkspaceRoot :: !(Maybe FilePath)
  -- ^ If set, restricts file operations to within this directory tree
  , policySensitivePatterns :: ![Text]
  -- ^ File names or substrings blocked from reading/writing (e.g. .env, id_rsa, keys)
  , policyCommandWhitelist :: !(Maybe [Text])
  -- ^ Optional strict whitelist of allowed command binaries
  , policyCommandBlacklist :: ![Text]
  -- ^ Blocked dangerous command substrings (e.g. rm -rf /, fork bombs)
  , policyApprovalHook :: !(Maybe (ApprovalRequest -> IO ApprovalDecision))
  -- ^ Optional Human-in-the-Loop approval callback for sensitive actions
  }
  deriving (Generic)

{- | Default standard security policy:

* Allows 'TierReadOnly', 'TierWorkspaceWrite', and 'TierExecution'.
* Protects sensitive secret files (.env, private keys, credential files).
* Blocks destructive shell command patterns.
-}
defaultSecurityPolicy :: SecurityPolicy
defaultSecurityPolicy =
  SecurityPolicy
    { policyAllowedTiers = Set.fromList [TierReadOnly, TierWorkspaceWrite, TierExecution]
    , policyWorkspaceRoot = Nothing
    , policySensitivePatterns =
        [ ".env"
        , "id_rsa"
        , "id_ed25519"
        , "id_ecdsa"
        , ".aws/credentials"
        , ".gemini_api_key"
        , "shadow"
        , "master.passwd"
        ]
    , policyCommandWhitelist = Nothing
    , policyCommandBlacklist =
        [ "rm -rf /"
        , "rm -fr /"
        , ":(){ :|:& };:"
        , "mkfs"
        , "dd if=/dev/zero"
        , "chmod -R 777 /"
        , "chown -R"
        , "> /dev/sda"
        ]
    , policyApprovalHook = Nothing
    }

-- | Restrictive read-only security policy (prevents any state mutations).
readOnlySecurityPolicy :: SecurityPolicy
readOnlySecurityPolicy =
  defaultSecurityPolicy
    { policyAllowedTiers = Set.singleton TierReadOnly
    }

-- | Sandbox security policy restricting operations strictly to a workspace directory.
workspaceSandboxPolicy :: FilePath -> SecurityPolicy
workspaceSandboxPolicy root =
  defaultSecurityPolicy
    { policyWorkspaceRoot = Just root
    }

-- | Unrestricted security policy allowing all tiers and bypassing sensitive file filters.
unrestrictedSecurityPolicy :: SecurityPolicy
unrestrictedSecurityPolicy =
  SecurityPolicy
    { policyAllowedTiers = Set.fromList [TierReadOnly, TierWorkspaceWrite, TierExecution, TierAdmin]
    , policyWorkspaceRoot = Nothing
    , policySensitivePatterns = []
    , policyCommandWhitelist = Nothing
    , policyCommandBlacklist = []
    , policyApprovalHook = Nothing
    }

-- | Check whether a tool tier is permitted under the policy.
checkTierPermission :: SecurityPolicy -> ToolTier -> Either LangchainError ()
checkTierPermission SecurityPolicy {..} tier
  | tier `Set.member` policyAllowedTiers = Right ()
  | otherwise =
      Left $
        toolError
          ( "Permission denied: Tool requires "
              <> T.pack (show tier)
              <> " authorization, which is not granted by the current security policy."
          )
          Nothing
          Nothing

-- | Check if a path refers to a sensitive file (keys, credentials, secrets).
isSensitivePath :: SecurityPolicy -> FilePath -> Bool
isSensitivePath SecurityPolicy {..} path =
  let lowerPath = map toLower (normalise path)
   in any (\pat -> T.unpack (T.toLower pat) `isInfixOf` lowerPath) policySensitivePatterns

-- | Validate that a file path remains safely within the configured workspace sandbox.
validateSandboxPath ::
  MonadIO m =>
  SecurityPolicy ->
  FilePath ->
  m (Either LangchainError FilePath)
validateSandboxPath SecurityPolicy {..} rawPath = liftIO $ do
  -- 1. Check sensitive path filter
  if isSensitivePath
    ( SecurityPolicy
        policyAllowedTiers
        policyWorkspaceRoot
        policySensitivePatterns
        policyCommandWhitelist
        policyCommandBlacklist
        policyApprovalHook
    )
    rawPath
    then
      pure $
        Left $
          toolError
            ("Access denied: Path matches sensitive/secret file policy: " <> T.pack rawPath)
            Nothing
            Nothing
    else case policyWorkspaceRoot of
      Nothing -> pure $ Right (normalise rawPath)
      Just root -> do
        canonRoot <- canonicalizePath root
        let targetPath =
              if isAbsolute rawPath
                then rawPath
                else canonRoot </> rawPath
        canonTarget <- canonicalizeAncestor targetPath
        let prefix = canonRoot ++ [pathSeparator]
            isInside = canonTarget == canonRoot || prefix `isPrefixOf` canonTarget
        if isInside
          then pure $ Right canonTarget
          else
            pure $
              Left $
                toolError
                  ( "Sandbox violation: Path '"
                      <> T.pack rawPath
                      <> "' escapes the workspace root '"
                      <> T.pack canonRoot
                      <> "'."
                  )
                  Nothing
                  Nothing
  where
    canonicalizeAncestor :: FilePath -> IO FilePath
    canonicalizeAncestor p = do
      ex <- doesFileExist p
      dx <- doesDirectoryExist p
      if ex || dx
        then canonicalizePath p
        else do
          let parent = takeDirectory p
          if parent == p
            then canonicalizePath p
            else do
              canonParent <- canonicalizeAncestor parent
              pure (canonParent </> takeFileName p)

-- | Validate that a shell command does not contain dangerous destructive patterns.
validateCommandSafety :: SecurityPolicy -> Text -> Either LangchainError ()
validateCommandSafety SecurityPolicy {..} cmd = do
  -- Check blacklist
  let lowerCmd = T.toLower cmd
  mapM_
    ( \bl ->
        if bl `T.isInfixOf` lowerCmd
          then
            Left $
              toolError
                ("Command blocked by security policy (prohibited pattern: '" <> bl <> "')")
                Nothing
                Nothing
          else Right ()
    )
    policyCommandBlacklist

  -- Check whitelist if present
  case policyCommandWhitelist of
    Nothing -> Right ()
    Just wl ->
      let firstWord = case T.words cmd of
            (w : _) -> w
            [] -> ""
       in if firstWord `elem` wl
            then Right ()
            else
              Left $
                toolError
                  ("Command '" <> firstWord <> "' is not in the authorized command whitelist.")
                  Nothing
                  Nothing

-- | Wrap a 'Tool m' with permission tier enforcement, sandboxing, and optional HITL approval.
withSecurity ::
  MonadIO m =>
  ToolTier ->
  SecurityPolicy ->
  Tool m ->
  Tool m
withSecurity tier policy tool@Tool {..} =
  tool
    { toolExecute = \args -> do
        case checkTierPermission policy tier of
          Left err -> pure $ Left err
          Right () -> do
            -- Check HITL approval if hook is defined
            mApproval <- case policyApprovalHook policy of
              Nothing -> pure Approved
              Just hook -> liftIO $ hook (ApprovalRequest toolName tier args toolDescription)
            case mApproval of
              Denied reason ->
                pure $
                  Left $
                    toolError ("Execution rejected by approval policy: " <> reason) (Just toolName) Nothing
              Approved -> toolExecute args
    }
