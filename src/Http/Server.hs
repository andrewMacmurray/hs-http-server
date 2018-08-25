{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server where

import qualified Control.Exception    as E
import           Control.Monad        (forever)
import qualified Data.ByteString      as B
import           Http.Server.Handler  (Handler, badRequest, serverError)
import           Http.Server.Request  (Request (..), parseRequest)
import           Http.Server.Response (Response (..), encodeResponse)
import           Http.Server.Socket   (ServerSocket (..), Socket (..), listenOn)

serve :: Int -> Handler -> IO ()
serve port handler = runServer handler serverSocket
  where
    serverSocket = listenOn port

runServer :: Handler -> IO ServerSocket -> IO ()
runServer handler serverSocket =
  E.bracket serverSocket closeServer loop
  where
    loop = forever . runRequest handler

runRequest :: Handler -> ServerSocket -> IO ()
runRequest handler ServerSocket {..} =
  E.bracket accept close execRequest
  where
    execRequest sock = do
      readRequest sock >>= (writeResponse sock handler)
      close sock

writeResponse :: Socket -> Handler -> Maybe Request -> IO ()
writeResponse Socket {..} handler req =
  encodeResponse <$> runHandler handler req >>= send

readRequest :: Socket -> IO (Maybe Request)
readRequest Socket {..} = parseRequest <$> receive maxBytes
  where
    maxBytes = 2024

runHandler :: Handler -> Maybe Request -> IO Response
runHandler handler req = execHandler `E.catch` errorResponse
  where
    errorResponse :: E.SomeException -> IO Response
    errorResponse = const serverError
    execHandler = maybe badRequest handler $ req
