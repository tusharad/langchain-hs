{-# LANGUAGE OverloadedStrings #-}

module App.OpenAI.ImageInput
  ( runApp
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Langchain.Error (toString)
import Langchain.LLM.Core
import qualified Langchain.LLM.Core as LLM
import Langchain.LLM.OpenAI
import qualified OpenAI.V1.Chat.Completions as CreateChat hiding
  ( ChatCompletionChunk (..)
  , ChatCompletionObject (..)
  )
import qualified OpenAI.V1.Models as Models
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

runApp :: IO ()
runApp = do
  mbApiKey <- lookupEnv "OPENAI_API_KEY"
  case mbApiKey of
    Nothing -> do
      putStrLn "Please provide OPENAI_API_KEY environment variable"
      exitFailure
    Just aKey -> do
      let openai =
            OpenAI
              { apiKey = T.pack aKey
              , callbacks = []
              , baseUrl = Nothing
              }
      singleImageDemo openai

safeReadFile :: FilePath -> IO (Either IOException BS.ByteString)
safeReadFile = try . BS.readFile

asPath :: FilePath -> IO (Maybe BS.ByteString)
asPath filePath = do
  exists <- doesFileExist filePath
  if exists
    then either (const Nothing) Just <$> safeReadFile filePath
    else return Nothing

supportedExtensions :: [String]
supportedExtensions = [".jpg", ".jpeg", ".png"]

isSupportedExtension :: FilePath -> Bool
isSupportedExtension p = map toLower (takeExtension p) `elem` supportedExtensions

encodeImage :: FilePath -> IO (Maybe Text)
encodeImage filePath = do
  if not (isSupportedExtension filePath)
    then return Nothing
    else do
      maybeContent <- asPath filePath
      return $ fmap (TE.decodeUtf8 . Base64.encode) maybeContent

singleImageDemo :: OpenAI -> IO ()
singleImageDemo openai = do
  let textPart = "What is in this image? Describe it in detail."
  let sampleImagePath = "./sample.png"
  mbBase64ImageData <- encodeImage sampleImagePath
  case mbBase64ImageData of
    Nothing -> do
      putStrLn "Could not read image path"
      exitFailure
    Just base64ImageData -> do
      let imageUrlPart =
            "data:image/"
              <> T.pack (drop 1 $ takeExtension sampleImagePath)
              <> ";base64,"
              <> base64ImageData
      let message =
            LLM.defaultMessage
              { LLM.role = User
              , LLM.content = textPart
              , LLM.messageData = LLM.defaultMessageData {messageImages = Just [imageUrlPart]}
              }
      let chatMessage = NE.singleton message

      putStrLn "Sending image to OpenAI for analysis..."
      result <-
        chat
          openai
          chatMessage
          ( Just $
              CreateChat._CreateChatCompletion
                { CreateChat.model = Models.Model "gpt-4o-mini"
                }
          )
      case result of
        Left err -> do
          putStrLn $ "Error: " <> toString err
          exitFailure
        Right (Message _ response _) -> do
          putStrLn "OpenAI's Analysis:"
          T.putStrLn response
