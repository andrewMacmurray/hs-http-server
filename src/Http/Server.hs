{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Http.Server where

import qualified Control.Exception    as E
import           Control.Monad        (forever)
import qualified Data.ByteString      as B
import           Http.Server.Handler
import           Http.Server.Request
import           Http.Server.Response
import           Http.Server.Socket
import           Network.HTTP.Types
import qualified Network.Socket       as NS

serve :: Int -> Handler -> IO ()
serve port handler = runServer handler $ listenOn port

runServer :: Handler -> IO ServerSocket -> IO ()
runServer handler serverSocket = E.bracket serverSocket close' loop
  where
    loop = forever . runRequest handler

runRequest :: Handler -> ServerSocket -> IO ()
runRequest handler ServerSocket {..} =
  E.bracket accept close execRequest
  where
    execRequest sock = do
      readRequest sock >>= (writeResponse sock handler)
      close sock

writeResponse :: Socket -> Handler -> Either String Request -> IO ()
writeResponse Socket {..} handler req =
  encodeResponse <$> runHandler handler req >>= send

readRequest :: Socket -> IO (Either String Request)
readRequest Socket {..} = parseRequest <$> receive maxBytes
  where
    maxBytes = 2024

runHandler :: Handler -> Either a Request -> IO Response
runHandler handler req = execHandler `E.catch` errorResponse
  where
    errorResponse :: E.SomeException -> IO Response
    errorResponse = const serverError
    execHandler = either (const badRequest) handler $ req
