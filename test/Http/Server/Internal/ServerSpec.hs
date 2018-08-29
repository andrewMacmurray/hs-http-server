{-# LANGUAGE OverloadedStrings #-}

module Http.Server.Internal.ServerSpec where

import           Control.Concurrent.MVar
import qualified Data.ByteString             as B
import           Data.Tuple                  (swap)
import qualified Http.Server                 as S
import           Http.Server.Handler         (respond, respondOk)
import           Http.Server.Internal.Socket
import           Test.Hspec

mockServerSocket :: IO Socket -> IO ServerSocket
mockServerSocket socket =
  return
    ServerSocket
    { accept = socket
    , closeServer = error "server socket should not have closed"
    }

mockSocket :: B.ByteString -> (B.ByteString -> IO ()) -> IO Socket
mockSocket input onSend = do
  remainingInput <- newMVar input
  return
    Socket
    { send = onSend
    , receive = receive remainingInput
    , close = return ()
    }
  where
    receive remainingInput maxBytes =
      modifyMVar remainingInput $ return . swap . B.splitAt maxBytes

spec :: Spec
spec =
  describe "runRequest" $ do
    it "reads a valid HTTP request from a socket and writes response" $ do
      let input = "GET / HTTP/1.1\r\n\r\n"
          output = "HTTP/1.1 200 OK\r\n\r\n"
          handler = respondOk
          socket = mockSocket input $ shouldBe output
      mockServerSocket socket >>= S.runRequest handler
    it "responds with bad request if send an invalid HTTP request" $ do
      let input = "Wut?"
          output = "HTTP/1.1 400 Bad Request\r\n\r\n"
          handler = respond "should not reach this handler"
          socket = mockSocket input $ shouldBe output
      mockServerSocket socket >>= S.runRequest handler
    it "responds with server error if handler throws an exception" $ do
      let input = "GET / HTTP/1.1\r\n\r\n"
          output = "HTTP/1.1 500 Internal Server Error\r\n\r\n"
          handler = const $ error "whoops"
          socket = mockSocket input $ shouldBe output
      mockServerSocket socket >>= S.runRequest handler
