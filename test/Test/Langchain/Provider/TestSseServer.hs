{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Langchain.Provider.TestSseServer
  ( withTestApplication
  , rawSseServer
  , capturingRawSseServer
  , sseFrame
  , gatedSseServer
  , cancellationAwareSseServer
  , collectModelStream
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, responseStream, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)

import Langchain.Core.Error (LangchainError)
import Langchain.Core.Model (ChatModel (..), Message)
import Langchain.Core.Stream (StreamEvent, collectEvents)

withTestApplication :: Application -> (T.Text -> IO a) -> IO a
withTestApplication app action =
  testWithApplication (pure app) $ \port ->
    action $ "http://127.0.0.1:" <> T.pack (show port)

rawSseServer :: [LBS.ByteString] -> Application
rawSseServer frames _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush ->
      mapM_ (\frame -> write (Builder.lazyByteString frame) >> flush) frames

capturingRawSseServer :: (LBS.ByteString -> IO ()) -> [LBS.ByteString] -> Application
capturingRawSseServer captureRequest frames request respond = do
  captureRequest =<< strictRequestBody request
  rawSseServer frames request respond

sseFrame :: LBS.ByteString -> LBS.ByteString
sseFrame payload = "data: " <> payload <> "\n\n"

gatedSseServer :: LBS.ByteString -> IO () -> [LBS.ByteString] -> Application
gatedSseServer firstFrame waitForContinuation remainingFrames _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
      write $ Builder.lazyByteString firstFrame
      flush
      waitForContinuation
      mapM_ (write . Builder.lazyByteString) remainingFrames
      flush

cancellationAwareSseServer :: LBS.ByteString -> IO () -> Application
cancellationAwareSseServer firstFrame signalClientClosed _request respond =
  respond $
    responseStream status200 [(hContentType, "text/event-stream")] $ \write flush -> do
      let keepAlive = do
            write ": keepalive\n\n"
            flush
            threadDelay 1000
            keepAlive
          onDisconnect :: SomeException -> IO ()
          onDisconnect _ = signalClientClosed
      write $ Builder.lazyByteString firstFrame
      flush
      keepAlive `catch` onDisconnect

collectModelStream ::
  ChatModel model =>
  model -> [Message] -> Maybe (ModelConfig model) -> IO (Either LangchainError [StreamEvent])
collectModelStream provider messages config =
  runResourceT $ runExceptT $ collectEvents (stream provider messages config)
