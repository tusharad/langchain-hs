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
import qualified Langchain.LLM.Internal.OpenAI as OpenAI
import Langchain.LLM.OpenAI
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
              , openAIModelName = "gpt-4o-mini"
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
  let textPart = OpenAI.TextContentPart "What is in this image? Describe it in detail."
  let sampleImagePath = "./sample.png"
  mbBase64ImageData <- encodeImage sampleImagePath
  case mbBase64ImageData of
    Nothing -> do
      putStrLn "Could not read image path"
      exitFailure
    Just base64ImageData -> do
      let imageUrlPart =
            OpenAI.ImageUrlContentPart
              ( "data:image/"
                  <> T.pack (drop 1 $ takeExtension sampleImagePath)
                  <> ";base64,"
                  <> base64ImageData
              )
      let message =
            OpenAI.defaultMessage
              { OpenAI.role = OpenAI.User
              , OpenAI.content = Just (OpenAI.ContentParts [textPart, imageUrlPart])
              }
      let chatMessage = NE.singleton $ LLM.from message

      putStrLn "\nSending image to OpenAI for analysis..."
      result <- chat openai chatMessage Nothing
      case result of
        Left err -> do
          putStrLn $ "Error: " <> toString err
          exitFailure
        Right (Message _ response _) -> do
          putStrLn "OpenAI's Analysis:"
          T.putStrLn response
